package com.qnfly.common.simhash

import org.slf4j.LoggerFactory

import scala.collection.mutable

/**
  * com.qnfly.common.simhash.SimhashIndex
  *
  * Created by ligz on 2017/2/3.
  */

class SimhashIndex(values: Iterable[(String, BigInt)], var f: Int = 64, var k: Int = 2) extends Serializable {

  val logger = LoggerFactory.getLogger(getClass)

  val count = values.toList.size

  // 将传入的simhash列表加到索引中
  values.zipWithIndex.foreach {
    case ((sid, sh), i) => {
      if ( i % 10000 == 0 || i == count - 1) {
        logger.info(s"添加索引进度：${i + 1}/$count")
      }
      add(sid, sh)
    }
  }

  /**
    * 计算两个simhash值的距离（汉明距离），
    * 也就是sh1和sh2进行xor后得到的二进制数中1的个数
    *
    * @param sh1
    * @param sh2
    * @return
    */
  def distance(sh1: BigInt, sh2: BigInt): Int = {
    var xor = (sh1 ^ sh2) & ((BigInt(1) << f) - 1)
    var cnt = 0
    while (xor != 0) {
      cnt += 1
      xor &= xor - 1
    }
    cnt
  }


  /**
    * 分组索引位置，64位的话应该是（0， 16， 32， 48）
    *
    * @return
    */
  val offsets = (0 until k).map(i => f / (k + 1) * i).toList


  /**
    * 获取simhash值，各个片段的key
    *
    * @param value
    * @return
    */
  def getKeys(value: BigInt) = {
    offsets.zipWithIndex.map { case (offset, i) =>
      val m = if (i == offsets.length - 1) {
        BigInt(2).pow(f - offset) - 1
      } else {
        BigInt(2).pow(offsets(i + 1) - offsets(i)) - 1
      }
      val c = (value >> offset) & m
      List(c, i.toString).mkString(":")
    }
  }


  /**
    * 分块桶，每个元素的key是simhash分段后每段的值，
    * value保存的是相同key对应的所有simhash与对应的id
    */
  val bucket = mutable.HashMap[String, mutable.Set[(String, BigInt)]]()


  /**
    * 向索引中添加simhash值
    *
    * @param sid
    * @param sh
    */
  def add(sid: String, sh: BigInt) = {
    getKeys(sh).foreach(key => {
      if (bucket.contains(key)) {
        bucket(key).add((sid, sh))
      } else {
        bucket(key) = mutable.Set[(String, BigInt)]((sid, sh))
      }
    })
  }


  /**
    * 在索引中删除特定simhash
    * @param sid
    * @param sh
    */
  def delete(sid: String, sh: BigInt) = {
    getKeys(sh).foreach(key => if (bucket.contains(key)) bucket(key).remove((sid, sh)))
  }


  /**
    * 获取key个数
    * @return
    */
  def bucketSize = bucket.size


  /**
    * 获取近似重复的simhash对应的id
    * @param sh
    * @return
    */
  def getNearDups(sh: BigInt, n: Int = k + 1) =
    getKeys(sh).filter(key => bucket.contains(key)).map(key => {
      val cans = bucket(key)
      if (cans.size > 200) {
//        logger.warn(s"发现容量超过200的桶，key=【$key】，len=【${cans.size}】")
        println(s"发现容量超过200的桶，key=【$key】，len=【${cans.size}】")
      }
      cans.filter {
        case (_, shI) => {
          val d = distance(sh, shI)
          d <= n
        }
      }
    }).flatMap(x => x.map(_._1)).toSet


  /**
    * 将另外一个索引合并
    * @param other
    * @return
    */
  def mergeOther(other: SimhashIndex): SimhashIndex = {
    other.bucket.foreach {
      case (key, values) => {
        if (bucket.contains(key)) bucket(key) ++= values
        else bucket(key) = values
      }
    }
    this
  }

}

object SimhashIndex {

  def apply(values: Iterable[(String, BigInt)]) = new SimhashIndex(values)

  def apply() = new SimhashIndex(List[(String, BigInt)]())

}
