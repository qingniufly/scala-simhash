import com.qnfly.common.simhash.{SimHash, SimhashIndex}

/**
  * SimHashTest
  *
  * Created by ligz on 2017/2/9.
  */
object SimHashTest extends App {

  val str = "我是蓝翔技工拖拉机学院手扶拖拉机专业的。不用多久，我就会升职加薪，当上总经理，出任CEO，走上人生巅峰。"

  val str1 = "我是蓝翔技工拖拉机学院手扶拖拉机专业的。用不了多久，我就会升职加薪，当上总经理，出任CEO，走上人生巅峰。"

  val simhash = SimHash("/Users/ligz/code/github-sources/simhash-1/dict/idf.utf8",
    "/Users/ligz/code/github-sources/simhash-1/dict/stop_words.utf8"
  )

  val topN = 15
  println(simhash.getHash(str, topN))
  println(simhash.getHash(str1, topN))


  val index = SimhashIndex()
  val dis = index.distance(simhash.getHash(str, topN), simhash.getHash(str1, topN))

  println(s"$str\n和\n$str1\n的距离是：$dis")

}
