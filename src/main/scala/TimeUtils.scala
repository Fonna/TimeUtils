import org.joda.time.{DateTime, Days}

import scala.collection.mutable.ArrayBuffer

/**
 * 日期相关组件
 */
object TimeUtils {

  def main(args: Array[String]): Unit = {
    println(getDaysOfMonth("201912").mkString(","))
//    println(dayOfWeek(20210910))
  }

  val MONTH_NUM_MAP = Map(1 -> 0, 2 -> 3, 3 -> 3, 4 -> 6, 5 -> 1, 6 -> 4, 7 -> 6, 8 -> 2, 9 -> 5, 10 -> 0, 11 -> 3, 12 -> 5)
  val CENTRURY_NUM = 6

  /**
   * 转为日期标准ISO8601格式
   *
   * @param date YYYYMMDD
   * @return yyyy-MM-dd
   */
  def toISODate(date: Int): String = {
    val day = date % 100
    val month = date / 100 % 100
    val year = date / 10000
    year + "-" + month.formatted("%02d") + "-" + day.formatted("%02d")
  }

  /**
   * 转为日期标准ISO8601格式
   *
   * @param date    支持YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @param reverse 日期是否是反转格式,如: 08/03/2018, 08-03-2018,默认是正常格式,如20180308 2018/03/08
   * @return yyyy-MM-dd
   */
  def toISODate(date: String, reverse: Boolean = false): String = {
    if(date.length < 8){
      throw new Exception("date formation error:" + date)
    }
    var dateStr = date
    if(reverse){
      dateStr = date.reverse
    }
    val sep = dateStr(4)
    if(dateStr.length == 8 && (sep >= '0' && sep <= '9')){
      dateStr.substring(0, 4) + "-" + dateStr.substring(4, 6) + "-" + dateStr.substring(6, 8)
    }else if(sep < '0' || sep > '9'){
      val nextPos = dateStr.indexOf(sep, 5)
      if(nextPos < 0 || nextPos + 1 > dateStr.length){
        throw new Exception("date formation error:" + date)
      }
      val month =dateStr.substring(5, nextPos)
      val day = dateStr.substring(nextPos + 1)
      if(month.isEmpty
        || day.isEmpty
        || month.toInt < 1
        || month.toInt > 12
        || day.toInt < 1
        || day.toInt > 31){
        throw new Exception("date formation error:" + date)
      }
      dateStr.replace(sep.toString, "-")
    }else
      throw new Exception("date formation error:" + date)
  }

  /**
   * 输入日期是星期几 如:20180308 -> 4  即周四
   * 使用基姆拉尔森计算公式
   * @param date YYYYMMDD
   * @return 周几，值一定落在集合{1, 2, 3, 4, 5, 6, 7}, eg. 1 -> Monday
   */
  def dayOfWeek(date: Int): Int = {
    val year = date / 10000 % 100
    val month = MONTH_NUM_MAP(date / 100 % 100)
    val day = date % 100
    val yearNum = year / 4
    val N = (CENTRURY_NUM + year + yearNum + month + day) % 7
    if (0 == N) 7
    else N
  }

  /**
   * 输入日期是星期几 如:2018-03-08 -> 4  即周四
   *
   * @param dateStr YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @return 周几，值一定落在集合{1, 2, 3, 4, 5, 6, 7}, eg. 1 -> Monday
   */
  def dayOfWeek(dateStr: String): Int = {
    val date = toISODate(dateStr)
    val year = date.slice(2, 4).toInt
    val month = date.slice(5, 7).toInt
    val day = date.slice(8, 10).toInt
    val yearNum = year / 4
    val monthNum = MONTH_NUM_MAP(month)
    val N = (CENTRURY_NUM + year + yearNum + monthNum + day) % 7
    if (0 == N) 7
    else N
  }

  /**
   * 返回上delta个周的自然周的第一天和最后一天 如:
   * (20180308,-2)-> (20180219,20180225)
   * (20180308,-1)-> (20180226,20180304)
   * (20180308,0) -> (20180305,20180311)
   * (20180308,1) -> (20180312,20180318)
   * (20180308,2) -> (20180319,20180325)
   *
   * @param date  YYYYMMDD
   * @param delta 上n周
   * @return 上个自然周周一、周日的日期(YYYYMMDD)
   */
  def getDeltaNaturalWeek(date: Int, delta: Int): (Int, Int) = {
    val day = new DateTime(toISODate(date))
    val Monday = day.plusWeeks(delta).minusDays(day.getDayOfWeek - 1).toString("yyyyMMdd").toInt
    val Sunday = day.plusWeeks(delta + 1).minusDays(day.getDayOfWeek).toString("yyyyMMdd").toInt
    (Monday, Sunday)
  }

  /**
   * 返回上delta个周的自然周的第一天和最后一天 如:
   * (2018-03-08,-2)-> (2018-02-19,2018-02-25)
   * (2018-03-08,-1)-> (2018-02-26,2018-03-04)
   * (2018-03-08,0) -> (2018-03-05,2018-03-11)
   * (2018-03-08,1) -> (2018-03-12,2018-03-18)
   * (2018-03-08,2) -> (2018-03-19,2018-03-25)
   *
   * @param dateStr YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @param delta 上n周
   * @return 上个自然周周一、周日的日期(yyyy-MM-dd)
   */
  def getDeltaNaturalWeek(dateStr: String, delta: Int): (String, String) = {
    val date = toISODate(dateStr)
    val day = new DateTime(date)
    val Monday = day.plusWeeks(delta).minusDays(day.getDayOfWeek - 1).toString("yyyy-MM-dd")
    val Sunday = day.plusWeeks(delta + 1).minusDays(day.getDayOfWeek).toString("yyyy-MM-dd")
    (Monday, Sunday)
  }

  /**
   * 返回上delta个月的自然月的第一天和最后一天 如:
   * (20180308,-2)) -> (20180101,20180131)
   * (20180308,-1) -> (20180201,20180228)
   * (20180308,0)  -> (20180301,20180331)
   * (20180308,1)  -> (20180401,20180430)
   * (20180308,2)  -> (20180501,20180531)
   *
   * @param date  YYYYMMDD
   * @param delta 上delta个月
   * @return 上delta个自然月第一天、最后一天的日期(YYYYMMDD)
   */
  def getDeltaNaturalMonth(date: Int, delta: Int): (Int, Int) = {
    val day = new DateTime(toISODate(date)).plusMonths(delta)
    val firstDay =day.toString("yyyyMM01").toInt
    val lastDay = new DateTime(toISODate(firstDay)).plusMonths(1).minusDays(1).toString("yyyyMMdd").toInt
    (firstDay, lastDay)
  }



  /**
   * 返回上delta个月的自然月的第一天和最后一天 如:
   * (2018-03-08,-2) -> (2018-01-01,2018-01-31)
   * (2018-03-08,-1) -> (2018-02-01,2018-02-28)
   * (2018-03-08,0)  -> (2018-03-01,2018-03-31)
   * (2018-03-08,1)  -> (2018-04-01,2018-04-30)
   * (2018-03-08,2)  -> (2018-05-01,2018-05-31)
   *
   * @param dateStr  YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @param delta 上delta个月
   * @return 上delta个自然月第一天、最后一天的日期(yyyy-MM-dd)
   */
  def getDeltaNaturalMonth(dateStr: String, delta: Int): (String, String) = {
    val date = toISODate(dateStr)
    val day = new DateTime(date).plusMonths(delta)
    val firstDayStr = day.toString("yyyy-MM-01")
    val lastDayStr = new DateTime(firstDayStr).plusMonths(1).minusDays(1).toString("yyyy-MM-dd")
    (firstDayStr, lastDayStr)
  }
  /**
   * 返回上delta个周的自然周的第一天和上一周的最后一天，注意与 getDeltaNaturalWeek 的区别 如:
   * (20180308,-2)-> (20180219,20180304)
   * (20180308,-1)-> (20180226,20180304)
   * (20180308,0) -> (20180305,20180311)
   * (20180308,1) -> (20180312,20180318)
   * (20180308,2) -> (20180312,20180325)
   *
   * @param date  YYYYMMDD
   * @param delta 上n周
   * @return 上个自然周周一、周日的日期(YYYYMMDD)
   */
  def getDeltaWeekRange(date: Int, delta: Int ): (Int, Int) = {
    val day = new DateTime(toISODate(date))
    if (delta >= 0) {
      val incremental = if (delta == 0) 0 else 1
      val Monday = day.plusWeeks(incremental).minusDays(day.getDayOfWeek - 1).toString("yyyyMMdd").toInt
      val Sunday = day.plusWeeks(delta + 1).minusDays(day.getDayOfWeek).toString("yyyyMMdd").toInt
      (Monday, Sunday)
    } else {
      val Monday = day.plusWeeks(delta).minusDays(day.getDayOfWeek - 1).toString("yyyyMMdd").toInt
      val Sunday = day.minusDays(day.getDayOfWeek).toString("yyyyMMdd").toInt
      (Monday, Sunday)
    }
  }


  /**
   * 返回上delta个周的自然周的第一天和上一周的最后一天 如:
   * (2018-03-08,-2)-> (2018-02-19,2018-03-04)
   * (2018-03-08,-1)-> (2018-02-26,2018-03-04)
   * (2018-03-08,0) -> (2018-03-05,2018-03-11)
   * (2018-03-08,1) -> (2018-03-12,2018-03-18)
   * (2018-03-08,2) -> (2018-03-12,2018-03-25)
   *
   * @param dateStr YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @param delta 上n周
   * @return 上个自然周周一、周日的日期(yyyy-MM-dd)
   */
  def getDeltaWeekRange(dateStr: String, delta: Int): (String, String) = {
    val date = toISODate(dateStr)
    val day = new DateTime(date)
    if (delta >= 0) {
      val incremental = if (delta == 0) 0 else 1
      val Monday = day.plusWeeks(incremental).minusDays(day.getDayOfWeek - 1).toString("yyyy-MM-dd")
      val Sunday = day.plusWeeks(delta + 1).minusDays(day.getDayOfWeek).toString("yyyy-MM-dd")
      (Monday, Sunday)
    } else {
      val Monday = day.plusWeeks(delta).minusDays(day.getDayOfWeek - 1).toString("yyyy-MM-dd")
      val Sunday = day.minusDays(day.getDayOfWeek).toString("yyyy-MM-dd")
      (Monday, Sunday)
    }
  }

  /**
   * 返回上delta个月的自然月的第一天和上月的最后一天 如:
   * (20180308,-2) -> (20180101,20180228)
   * (20180308,-1) -> (20180201,20180228)
   * (20180308,0)  -> (20180301,20180331)
   * (20180308,1)  -> (20180401,20180430)
   * (20180308,2)  -> (20180401,20180531)
   *
   * @param date  YYYYMMDD
   * @param delta 上delta个月
   * @return 上delta个自然月第一天、最后一天的日期(YYYYMMDD)
   */
  def getDeltaMonthRange(date: Int, delta: Int): (Int, Int) = {
    val day = new DateTime(toISODate(date))
    if (delta >= 0) {
      val incremental = if (delta == 0) 0 else 1
      val firstDay = day.plusMonths(incremental).toString("yyyyMM01").toInt
      val lastDay = day.plusMonths(delta + 1).minusDays(day.getDayOfMonth).toString("yyyyMMdd").toInt
      (firstDay, lastDay)
    } else {
      val firstDay = day.plusMonths(delta).toString("yyyyMM01").toInt
      val lastDay = day.minusDays(day.getDayOfMonth).toString("yyyyMMdd").toInt
      (firstDay, lastDay)
    }
  }

  /**
   * 返回上delta个月的自然月的第一天和最后一天 如:
   * (2018-03-08,-2) -> (2018-01-01,2018-02-28)
   * (2018-03-08,-1) -> (2018-02-01,2018-02-28)
   * (2018-03-08,0)  -> (2018-03-01,2018-03-31)
   * (2018-03-08,1)  -> (2018-04-01,2018-04-30)
   * (2018-03-08,2)  -> (2018-04-01,2018-05-31)
   *
   * @param dateStr YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @param delta 上delta个月
   * @return 上delta个自然月第一天、最后一天的日期(yyyy-MM-dd)
   */
  def getDeltaMonthRange(dateStr: String, delta: Int): (String, String) = {
    val date = toISODate(dateStr)
    val day = new DateTime(date)
    if (delta >= 0) {
      val incremental = if (delta == 0) 0 else 1
      val firstDay = day.plusMonths(incremental).toString("yyyy-MM-01")
      val lastDay = day.plusMonths(delta + 1).minusDays(day.getDayOfMonth).toString("yyyy-MM-dd")
      (firstDay, lastDay)
    } else {
      val firstDay = day.plusMonths(delta).toString("yyyy-MM-01")
      val lastDay = day.minusDays(day.getDayOfMonth).toString("yyyy-MM-dd")
      (firstDay, lastDay)
    }
  }


  /**
   * 返回上delta个月的月份形式 如:20180308,delta=1 ->  201802
   *
   * @param date  YYYYMMDD
   * @param delta 上几个月,默认n=1,即上一个月
   * @return YYYYMM
   */
  def getDeltaMonthCut(date: Int, delta: Int = -1): Int = {
    val day = new DateTime(toISODate(date))
    day.plusMonths(delta).toString("yyyyMM").toInt
  }


  /**
   * 返回上delta个月的月份形式 如:20180308,delta=1 ->  201802
   *
   * @param dateStr YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @param delta 上几个月
   * @return YYYYMM
   */
  def getDeltaMonthCut(dateStr: String, delta: Int): String = {
    val day = new DateTime(toISODate(dateStr))
    day.plusMonths(delta).toString("yyyyMM")
  }

  /**
   * 返回具体日期，如（20180308，-1）-> 20180307,（20180308，0）-> 20180308,（20180308，1）-> 20180309,
   *
   * @param date  YYYYMMDD
   * @param delta 间隔的天数
   * @return YYYYMMDD
   */
  def getDeltaDay(date: Int, delta: Int): Int = {
    val day = new DateTime(toISODate(date))
    day.plusDays(delta).toString("yyyyMMdd").toInt
  }


  /**
   * 返回具体日期，如（2018-03-08，-1）-> 2018-03-07,（2018-03-08，0）-> 2018-03-08,（2018-03-08，1）-> 2018-03-09,
   *
   * @param date  ISO日期格式,yyyy-MM-dd
   * @param delta 间隔的天数
   * @return YYYYMMDD
   */
  def getDeltaDay(date: String, delta: Int): String = {
    val day = new DateTime(toISODate(date))
    day.plusDays(delta).toString("yyyy-MM-dd")
  }

  /**
   * 返回具体日期，如（20180308，-1）-> 20180307,（20180308，0）-> 20180308,（20180308，1）-> 20180309,
   *
   * @param delta 返回与今天间隔的天数
   * @return YYYYMMDD
   */
  def getDeltaDay(delta: Int): Int = {
    val day = new DateTime()
    day.plusDays(delta).toString("yyyyMMdd").toInt
  }


  /**
   * 从开始日期和结合日期获取日期列表,如(2018-04-10, 2018-04-13) -> (2018-04-10,2018-04-11,2018-04-12,2018-04-13)
   *
   * @param startDayStr 开始日期,YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @param endDayStr   结束日期,YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @return 日期列表(字符串类型)
   */
  def getRangeDays(startDayStr: String, endDayStr: String): Array[String] = {
    val startDay = toISODate(startDayStr)
    val endDay = toISODate(endDayStr)
    val startTime = new DateTime(startDay)
    val endTime = new DateTime(endDay)
    val numberOfDays = Days.daysBetween(startTime, endTime).getDays
    val step = if(numberOfDays + 1 > 0) 1 else -1
    (0 to numberOfDays by step).map(startTime.plusDays(_).toString("yyyy-MM-dd")).toArray
  }

  /**
   * 从开始日期和结束日期获取日期列表,如(20180410, 20180413) -> (20180410,20180411,20180412,20180413)
   *
   * @param startDay 开始日期,YYYYMMDD
   * @param endDay   结束日期,YYYYMMDD
   * @return 日期列表(整数类型)
   */
  def getRangeDays(startDay: Int, endDay: Int): Array[Int] = {
    val startTime = new DateTime(toISODate(startDay))
    val numberOfDays = getDelta(startDay, endDay)
    val step = if(numberOfDays + 1 > 0) 1 else -1
    (0 to numberOfDays by step).map(startTime.plusDays(_).toString("yyyyMMdd").toInt).toArray
  }


  /**
   * 以开始日期与相邻天数确定日期列表,如(2018-04-11, 3) -> (2018-04-11,2018-04-12,2018-04-13,2018-04-14)
   *                              (2018-04-11, -3)-> (2018-04-11,2018-04-10,2018-04-09,2018-04-08)
   * @param startDayStr YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @param delta 相邻天数,支持负数
   * @return 日期列表
   */
  def getDeltaRangeDays(startDayStr:String, delta:Int):Array[String] = {
    val startDay = toISODate(startDayStr)
    val endDay = new DateTime(startDay).plusDays(delta).toString("yyyy-MM-dd")
    getRangeDays(startDay, endDay)
  }


  /**
   * 以开始日期与相邻天数确定日期列表,如(20180411, 3) -> (20180411,20180412,20180413,20180414
   *                              (20180411, -3)-> (20180411,20180410,20180409,20180408)
   * @param startDay 开始日期,YYYYMMDD
   * @param delta 相邻天数,支持负数
   * @return 日期列表
   */
  def getDeltaRangeDays(startDay:Int, delta:Int):Array[Int] = {
    val endDay = new DateTime(toISODate(startDay)).plusDays(delta).toString("yyyyMMdd").toInt
    getRangeDays(startDay, endDay)
  }

  /**
   * 获取制定月份的所有日期
   *
   * @param month 支持 YYYYMM 或 YYYYMMDD
   * @return [20180201,20180202,...,201802028] 从小到大排序
   */
  def getDaysOfMonth(month: Int): Array[Int] = {
    val start = if (month < 1000000) month * 100 + 1 else month / 100 * 100 + 1
    val startDay = new DateTime(toISODate(start))
    val endDay = startDay.plusMonths(1).minusDays(1)
    getRangeDays(startDay.toString("yyyyMMdd").toInt, endDay.toString("yyyyMMdd").toInt)
  }

  /**
   * 获取制定月份的所有日期
   *
   * @param month 支持 YYYYMM 或 YYYYMMDD
   * @return [20180201,20180202,...,201802028] 从小到大排序
   */
  def getDaysOfMonth(monthStr: String): Array[String] = {
    val month = monthStr.toInt
    val start = if (month < 1000000) month * 100 + 1 else month / 100 * 100 + 1
    val startDay = new DateTime(toISODate(start))
    val endDay = startDay.plusMonths(1).minusDays(1)
    getRangeDays(startDay.toString("yyyy-MM-dd"), endDay.toString("yyyy-MM-dd"))
  }

  /**
   * 从开始日期和结束日期获取月份列表,eg. (20180410, 20180613) -> (201804,201805,201806)
   *
   * @param start YYYYMMDD
   * @param end   YYYYMMDD
   * @return [201712,201801,201802]
   */
  def getRangeMonths(start: Int, end: Int): Array[Int] = {
    val startDay = new DateTime(toISODate(start))
    val endDay = new DateTime(toISODate(end))
    val monthList = new ArrayBuffer[String]
    var index = 0
    var monthStr = startDay.plusMonths(index).toString("yyyyMM")
    val endMonth = endDay.toString("yyyyMM")
    while (monthStr <= endMonth) {
      index += 1
      monthList.append(monthStr)
      monthStr = startDay.plusMonths(index).toString("yyyyMM")
    }
    monthList.map(_.toInt).toArray
  }


  /**
   * 从开始日期和结束日期获取月份列表,eg. (2018-04-10, 2018-06-13) -> (201804,201805,201806)
   *
   * @param startStr YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @param endStr   YYYYMMDD,YYYY/MM/DD,YYYY-MM-DD,YYYY%MM%DD,DD%MM%YYYY,DD/MM/YYYY,DD-MM-YYYY
   * @return [201712,201801,201802]
   */
  def getRangeMonths(startStr: String, endStr: String): Array[String] = {
    val start = toISODate(startStr)
    val end = toISODate(endStr)
    val startDay = new DateTime(start)
    val endDay = new DateTime(end)
    val monthList = new ArrayBuffer[String]
    var index = 0
    var monthStr = startDay.plusMonths(index).toString("yyyyMM")
    val endMonth = endDay.toString("yyyyMM")
    while (monthStr <= endMonth) {
      index += 1
      monthList.append(monthStr)
      monthStr = startDay.plusMonths(index).toString("yyyyMM")
    }
    monthList.toArray
  }


  /**
   * 从开始日期和结束日期获取周中和周末的分组
   *
   * @param start YYYYMMDD
   * @param end   YYYYMMDD
   * @return
   */
  def getWeekGroup(start: Int, end: Int): (Array[Int], Array[Int]) = {
    val weekDays = new ArrayBuffer[Int]()
    val weekEnds = new ArrayBuffer[Int]()
    getRangeDays(start, end).foreach(x => {
      val day = new DateTime(toISODate(x))
      if (day.dayOfWeek.get() < 6) weekDays.append(x)
      else weekEnds.append(x)
    })
    (weekDays.toSet.toArray, weekEnds.toSet.toArray)
  }


  /**
   * 查看两个日期相差多少天 - 支持负数，
   *
   * @param startDay 开始日期 YYYYMMDD
   * @param endDay   结束日期 YYYYMMDD
   * @return 间隔天数
   */
  def getDelta(startDay: Int, endDay: Int): Int = {
    val startTime = new DateTime(toISODate(startDay))
    val endTime = new DateTime(toISODate(endDay))
    Days.daysBetween(startTime, endTime).getDays
  }


  /**
   * 查看两个日期相差多少天 - 支持负数，
   * eg(2017-01-01,2017-01-31) -> 30
   *
   * @param startDay 开始日期 ISO日期格式,yyyy-MM-dd
   * @param endDay   结束日期 ISO日期格式,yyyy-MM-dd
   * @return 间隔天数
   */
  def getDelta(startDay: String, endDay: String): Int = {
    val startTime = new DateTime(toISODate(startDay))
    val endTime = new DateTime(toISODate(endDay))
    Days.daysBetween(startTime, endTime).getDays
  }

}
