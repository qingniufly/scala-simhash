import com.qnfly.common.simhash.{SimHash, SimhashIndex}

/**
  * SimHashTest
  *
  * Created by ligz on 2017/2/9.
  */
object SimHashTest extends App {


  val idfPath = getClass.getResource("/idf.utf8").getPath
  val stopwordsPath = getClass.getResource("/stop_words.utf8").getPath
  val simhash = SimHash(idfPath, stopwordsPath)

  val topN = 15
  val str = "我是蓝翔技工拖拉机学院手扶拖拉机专业的。不用多久，我就会升职加薪，当上总经理，出任CEO，走上人生巅峰。"
  val str1 = "用不了多久，我就会升职加薪，当上总经理，出任CEO，走上人生巅峰。"
  println(simhash.getHash(str, topN))
  println(simhash.getHash(str1, topN))
  println(s"$str\n和\n$str1\n的距离是：$dis")

  val index = SimhashIndex()
  val dis = index.distance(simhash.getHash(str, topN), simhash.getHash(str1, topN))

  //  val s1 = "功夫五级"
  //  val s2 = "尖刀连五级"
  //  val s1 = "我觉得好有聊"
  //  val s2 = "你聊天聊到可以了"
  val s1 = "以旧换新怎么换"
  val s2 = "我想换个新手机"

  val k1 = simhash.extract(s1, topN)
  val k2 = simhash.extract(s2, topN)
  println(s"$s1 关键词结果: $k1")
  println(s"$s2 关键词结果: $k2")

  val d = index.distance(simhash.getHash(s1, topN), simhash.getHash(s2, topN))
  println(s"$s1 与 $s2 的距离是:$d")


}
