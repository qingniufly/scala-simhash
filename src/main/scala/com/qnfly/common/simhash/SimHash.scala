package com.qnfly.common.simhash

import java.nio.charset.StandardCharsets

import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

/**
  * com.xiaoi.common.simhash.SimHash
  *
  * Created by ligz on 2017/2/8.
  */
class SimHash(idfPath: String, stopwordsPath: String) {

  val logger = LoggerFactory.getLogger(getClass)

  // simhash值位数
  val BIT_LEN = 64

  val hasher = new JenkinsHash()

  val keywordExtractor = new KeyWordExtractor(idfPath, stopwordsPath)

  /**
    * 提取关键词和权重，权重取TF-IDF
    * @param text
    * @param topN
    * @return
    */
  def extract(text: String, topN: Int): List[(String, Double)] = {
    keywordExtractor.extract(text, topN)
//    val wordWeights = keywordExtractor.extract(text, topN)
//    wordWeights.foreach(println)
//    wordWeights
  }

  /**
    * Hash函数，计算给定text的hash值
    * @param text
    * @return
    */
  def hash(text: String): BigInt = {
    hasher.hash64(text.getBytes(StandardCharsets.UTF_8))
  }


  /**
    * 得到关键词对应的hash值和TF-IDF值作为权重
    * @param text
    * @param topN
    * @return
    */
  private def getHashWeight(text: String, topN: Int): List[(BigInt, Double)] = {
    extract(text, topN).map {
      case (word, weight) => (hash(word), weight)
    }
  }

  /**
    * 计算SimHash值
    * 按位乘以权重，相应位为1，则权重为1 * weight，相应位为0，则权重为-1 * weight
    * 之后对各个关键词按位求和，结果中的每一位权重值 > 0, 置为1，否则置为0
    * @param text
    * @param topN
    * @return
    */
  def getHash(text: String, topN: Int): BigInt = {
    val hashWeight = getHashWeight(text, topN)
    val tmp = BigInt(1)

    val bitList = ListBuffer((0 until BIT_LEN).map(_ => 0.0): _*)
    hashWeight.foreach {
      case (hash, weight) =>
        (0 until BIT_LEN).foreach(i => {
          if (((tmp << i) & hash) != 0) {
            bitList(i) += weight
          }
          else {
            bitList(i) -= weight
          }
        })
    }
    var result = BigInt(0)
    val const1 = BigInt(1)
    (0 until BIT_LEN).foreach(i => {
      if (bitList(i) > 0) {
        result |= (const1 << i)
      }
    })
    result
  }


}


object SimHash {

  def apply(idfPath: String, stopwordsPath: String): SimHash =
    new SimHash(idfPath, stopwordsPath)

  def binaryString2Long(str: String) = {
    var result = BigInt(0)
    (0 until str.length).foreach(i => {
      result <<= 1
      if (str(i) - '1' == 0) {
        result += 1
      }
    })
    result
  }

}
