package come.abide.financial.gianlo.workshop

/**
  * Created by Matthew.McGowan on 26/07/2016.
  */
object StreamReader {
  def main(args: Array[String]) {
    val sparkConf = new SparkConf()
    val ssc = new StreamingContext(sparkConf, Seconds(5))

    val kStream = KinesisUtils.createStream(ssc, "gianloWorkshop", "TestStream", "http://")
  }
}
