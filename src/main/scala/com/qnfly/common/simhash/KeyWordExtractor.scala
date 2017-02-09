package com.qnfly.common.simhash

import java.io.StringReader

import org.lionsoul.jcseg.tokenizer.core.{DictionaryFactory, JcsegTaskConfig, SegmentFactory}
import org.slf4j.LoggerFactory

import scala.collection.mutable.{ListBuffer, Map => MMap, Set => MSet}
import scala.io.Source

/**
  * Created by ligz on 2017/2/8.
  */
class KeyWordExtractor(idfPath: String, stopwordsPath: String) {

  val logger = LoggerFactory.getLogger(getClass)

  val jcseg = {
    val config = new JcsegTaskConfig(getClass.getResourceAsStream("jcseg.properties"))
    val dic = DictionaryFactory.createSingletonDictionary(config)
    SegmentFactory.createJcseg(JcsegTaskConfig.COMPLEX_MODE,
      List[Object](config, dic):_*)
  }

  val idfMap = MMap[String, Double]()

  val stopwords = MSet[String]()

  var idfAvg = 0.0

  var wordsCnt = 0

  Source.fromFile(idfPath).getLines().foreach(line => {
    val pair = line.split(" ")
    if (pair.size < 2) {
      logger.warn(s"IDF文件中有异常，行号：${wordsCnt + 1}, 内容：$line")
    } else {
      wordsCnt += 1
      idfMap += pair(0) -> pair(1).toDouble
      idfAvg += pair(1).toDouble
    }
  })
  idfAvg /= wordsCnt
  logger.info(s"从IDF文件中加载单词个数：${wordsCnt}, IDF平均值：$idfAvg")

  var stopwordsCnt = 0
  Source.fromFile(stopwordsPath).getLines().foreach(line => {
    stopwords += line
    stopwordsCnt += 1
  })
  logger.info(s"从停用词文件中加载总个数：$stopwordsCnt")


  def seg(text: String): List[String] = {
    jcseg.reset(new StringReader(text))
    val words = ListBuffer[String]()
    var word = jcseg.next()
    while (word != null) {
      words += word.getValue
//      println(word.getValue)
      word = jcseg.next
    }
    words.toList
  }


  def extremeN[T](n: Int, li: List[T])
                          (comp: (T, T) => Boolean, revComp: (T, T) => Boolean): List[T] = {
    def updateSofar(sofar: List[T], el: T): List[T] =
      if (comp(el, sofar.head))
        (el :: sofar.tail).sortWith(revComp(_, _))
      else sofar

    (li.take(n).sortWith(revComp(_, _)) /: li.drop(n)) (updateSofar(_, _)).sortWith(comp(_, _))
  }

  def top[T](n: Int, li: List[T])
                    (implicit ord: Ordering[T]): List[T] =
    extremeN(n, li)(ord.gt(_, _), ord.lt(_, _))

  def bottom[T](n: Int, li: List[T])
                       (implicit ord: Ordering[T]): List[T] =
    extremeN(n, li)(ord.lt(_, _), ord.gt(_, _))


  def extract(text: String, topN: Int): List[(String, Double)] = {
    val words = seg(text)
    val tf = MMap[String, Double]()
    words.foreach(word => {
      if (!stopwords.contains(word)) {
        if (tf.contains(word)) tf(word) += 1.0
        else tf += word -> 1.0
      }
    })
    val tfidf = tf.map {
      case (word, tf) => {
        val weight = if (idfMap.contains(word)) tf * idfMap(word) else tf * idfAvg
        word -> weight
      }
    }.toList
    implicit object WordWeightOrd extends Ordering[(String, Double)] {
      override def compare(x: (String, Double), y: (String, Double)) = {
        val d = x._2 - y._2
        if (d > 0.0) 1
        else if (d < 0.0) -1
        else 0
      }
    }
    top(topN, tfidf)
  }



}
