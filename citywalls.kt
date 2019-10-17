import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import java.io.File
import java.lang.NumberFormatException
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.cbor.CBORFactory
import com.fasterxml.jackson.module.kotlin.registerKotlinModule
import org.jsoup.HttpStatusException
import org.jsoup.helper.HttpConnection


const val streetIdPattern = "street(\\d+)\\.html"
const val houseIdPattern = "house(\\d+)\\.html"
const val styleIdPattern = "style(\\d+)\\.html"
const val architectIdPattern = "architect(\\d+)\\.html"

fun parseStreetUrls(): List<String> {
    val streetUrls = ArrayList<String>();
    getDoc("http://www.citywalls.ru/street_index.html")
        .select(".m_content3 a").forEach {
            val mAttr = it?.attr("href");
            if (mAttr != null && mAttr.startsWith("http://")) {
                streetUrls.add(mAttr);
            }
        }
    return streetUrls
}

fun String.parseId(pattern: String): Int? {
    try {
        val idMatch = Regex(pattern).find(this)?.groupValues?.elementAtOrNull(1)
        return Integer.parseInt(idMatch)
    } catch (e: NumberFormatException) {
        return null
    }
}

fun ArrayList<Element>.firstAttr(attr: String): String? {
    return firstOrNull()?.attr(attr)
}

var conn = HttpConnection()

fun getDoc(url: String, retry: Int = 0): Document {
    try {
        return conn.url(url).get()
    } catch (e: Exception) {
        val sleep = Math.pow(2.0, retry.toDouble()).toLong()
        println("Caught exception $e, sleeping for $sleep seconds ...")
        Thread.sleep(1000 * sleep)
        conn = HttpConnection()
        conn.url(url)
        return getDoc(url, retry + 1)
    }
}

fun parseHouse(
    houseEl: Element,
    allStyles: HashMap<Int, String>,
    allArchitects: HashMap<Int, String>
): Pair<Int, House>? {
    val name = houseEl.select("h2").firstOrNull()?.text()?.trim();
    val imgSrc = houseEl.select("img").firstAttr("src");
    val houseUrl = houseEl.select("a.imb_more").firstAttr("href")
    val houseId = houseUrl?.parseId(houseIdPattern)
    val viewCount = houseEl.select("a.imb_eye").firstOrNull()?.text()?.trim()?.toIntOrNull();
    val commCount = houseEl.select("a.imb_comm").firstOrNull()?.text()?.trim()?.toIntOrNull();
    val addrEl = houseEl.select("div.address").firstOrNull();
    val addresses: List<Pair<Int, String>>;
    if (addrEl != null) {
        val streetIds: List<Int> = addrEl.select("a").map {
            it.attr("href").parseId(streetIdPattern) ?: -1
        }
        val houseIds: List<String> = addrEl.textNodes().map {
            it.text().replaceFirst(Regex("^\\s*,"), "").trim()
        }
        addresses = streetIds.zip(houseIds)
    } else {
        addresses = ArrayList()
    }
    val metaKeys = houseEl.select("div.info table td.item").map {
        it.text().replace(Regex(":\\s*$"), "").trim()
    }
    val metaVals = houseEl.select("div.info table td.value")
    val meta = HashMap<String, Element>()
    meta.putAll(metaKeys.zip(metaVals))
    val years = meta.get("Годы постройки")?.text()?.trim() ?: "";
    val styles = ArrayList<Int>()
    val architects = ArrayList<Int>()
    meta.get("Стиль")?.select("a")?.forEach {
        val styleId = it.attr("href").parseId(styleIdPattern)
        if (styleId != null) {
            allStyles.put(styleId, it.text().trim())
            styles.add(styleId)
        }
    }
    meta.get("Архитекторы")?.select("a")?.forEach {
        val architectId = it.attr("href").parseId(architectIdPattern)
        if (architectId != null) {
            allArchitects.put(architectId, it.text().trim())
            architects.add(architectId)
        }
    }
    if (name != null && houseId != null && viewCount != null && commCount != null) {
        return Pair(houseId, House(addresses, name, years, architects, styles, imgSrc, viewCount, commCount))
    }
    return null
}

fun parseStreetData(
    doc: Document,
    houses: HashMap<Int, House>,
    styles: HashMap<Int, String>,
    architects: HashMap<Int, String>
) {
    doc.select("div.cssHouseHead").forEach {
        val houseP = parseHouse(it, styles, architects)
        if (houseP != null) {
            val house = houseP.second
            houses.put(houseP.first, house)
        } else {
            println("Failed to parse house")
        }
    }
    var nextUrl = doc.select("a.right").firstAttr("href");
    if (nextUrl != null) {
        if (nextUrl.startsWith("/")) {
            nextUrl = "http://www.citywalls.ru$nextUrl"
        }
        parseStreetData(getDoc(nextUrl), houses, styles, architects)
    }
}

fun parseRegistry(): Registry {
    val streets = HashMap<Int, String>()
    val houses = HashMap<Int, House>()
    val styles = HashMap<Int, String>()
    val architects = HashMap<Int, String>()
    var i = 0;
    val streetUrls = parseStreetUrls()
    for (it in streetUrls) {
        i++
        val doc = getDoc(it);
        val name = (doc.select("div.title").text() ?: "")
            .substringBeforeLast('-')
            .trim();
        val id = it.parseId(streetIdPattern)
        if (name.isEmpty() || id == null) {
            println("Failed to parse street $it (id $id)")
        } else {
            streets.put(id, name)
            parseStreetData(doc, houses, styles, architects)
            println("[$i / ${streetUrls.size}] $id $name")
        }
    }
    return Registry(streets, houses, styles, architects)
}

data class House
    (
    val addresses: List<Pair<Int, String>>
    , val name: String
    , val years: String
    , val architects: List<Int>
    , val styles: List<Int>
    , val imgUrl: String?
    , val viewCount: Int
    , val commCount: Int
)

data class Registry
    (
    val streets: Map<Int, String>
    , val houses: Map<Int, House>
    , val styles: Map<Int, String>
    , val architects: Map<Int, String>
)

fun crawlData() {
    val mapper = ObjectMapper(JSONFactory()).registerKotlinModule()
    val data = parseRegistry()
    mapper.writeValue(File("citywalls.json"), data)
}

fun main() {
    crawlData()
}