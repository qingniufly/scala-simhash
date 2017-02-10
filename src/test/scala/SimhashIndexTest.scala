import com.qnfly.common.simhash.{SimHash, SimhashIndex}

import scala.io.Source

/**
  * SimhashIndexTest
  *
  * Created by ligz on 2017/2/4.
  */
object SimhashIndexTest extends App {


  def tick() = System.currentTimeMillis()


  def recTime[T](funInfo: String)(op: => T) = {
    val start = tick()
    val result = op
    val tk = tick()
    println(s"$funInfo 耗时：${tk - start}ms")
    result
  }

  val values = List[(String, BigInt)](
    ("1", SimHash.binaryString2Long("100010110110")),
    ("2", SimHash.binaryString2Long("110001110011")),
    ("3", SimHash.binaryString2Long("110001111111"))
  )

  val index = SimhashIndex()
  val dis = index.distance(SimHash.binaryString2Long("100010110110"),
    SimHash.binaryString2Long("110001110011"))

  values.foreach { case (id, hash) => index.add(id, hash) }

  val nid = index.getNearDups(SimHash.binaryString2Long("111010110111"))
  println(nid)

  println(s"100010110110\n与\n110001110011\n的距离是：$dis")


  val idfPath = getClass.getResource("/idf.utf8").getPath
  val stopwordsPath = getClass.getResource("/stop_words.utf8").getPath
  val taskFile = getClass.getResource("/task.txt").getPath

  val simhash = SimHash(idfPath, stopwordsPath)
  val topN = 15
  val tasks = Source.fromFile(taskFile).getLines().toList

  var cnt = 0
  val total = tasks.size

  val hashQs = recTime("计算Simhash值") {
    tasks.map(q => {
      cnt += 1
      if (cnt % 1000 == 0 || cnt == total) println(s"处理进度：$cnt/$total")
      q -> simhash.getHash(q, topN)
    })
  }


  val index1 = SimhashIndex()
  val index2 = SimhashIndex()
  val hashes = hashQs.zipWithIndex

  recTime("添加SimHashIndex") {
    hashes.take(300000).map(_.swap).foreach {
      case (i, (q, sh)) => index1.add(i.toString, sh)
    }
  }
  recTime("添加SimHashIndex") {
    hashes.takeRight(hashes.size - 300000).map(_.swap).foreach {
      case (i, (q, sh)) => index2.add(i.toString, sh)
    }
  }

  recTime("查询重复问题") {
    val s1 = "我是蓝翔技工拖拉机学院手扶拖拉机专业的。不用多久，我就会升职加薪，当上总经理，出任CEO，走上人生巅峰。"
    val sh1 = simhash.getHash(s1, topN)
    var ids = index1.getNearDups(sh1, 5)
    println("在前端查找重复，结果：" + ids.toList)
    ids = index2.getNearDups(sh1, 5)
    println("在后端查找重复，结果：" + ids.toList)
    val idx = index1.mergeOther(index2)
    ids = idx.getNearDups(sh1, 5)
    println("合并后查找重复，结果：" + ids.toList)

  }


  //  recTime("添加SimHashIndex") {
  //    hashQs.zipWithIndex.map(_.swap).foreach {
  //      case (i, (q, sh)) => index.add(i.toString, sh)
  //    }
  //  }

  //  recTime("查询重复问题") {
  //    val s1 = "系统一键恢复后分区会变吗"
  //    val sh1 = simhash.getHash(s1, topN)
  //    val ids = index.getNearDups(sh1, 5)
  //    println(ids.toList)
  //  }

}
