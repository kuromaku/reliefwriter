package reliefwriter
import java.io.File
import java.io.FileWriter
import scala.io.Source
object CharFreqCalculator {
  def getFiles(dir:File) : List[File] = {
    val rawlist = dir.listFiles()
    val filtered = rawlist.filter(x => (x.isFile && x.getName.takeRight(5) == "scala"))
    val dirs = rawlist.filter(_.isDirectory)
    var sets = List[File]()
    for (i <- 0 until dirs.length) {
      sets = getFiles(dirs(i)) ++ sets
    }
    filtered.toList ++ sets
    
  }
  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
    val path = args(0) //should be the directory which contains scala-files
    val files = getFiles(new File(path))
    var c = 0
    val num = 128
    val freqArray = new Array[Int](num)
    
    var errors = 0
    for (f <- files) {
      val src = Source.fromFile(f)
      while (src.hasNext) {
        c = src.next.toInt
        if (c < num) {
          freqArray(c) += 1
        }
      }
      src.close
    }
    val fw = new FileWriter("./charfreqs.txt")
    for (i <- 0 until num) {
      val freq = freqArray(i)
      if (freq > 0)
      fw.write(i+" "+freq+"\n")
    }
    fw.flush
    fw.close
  }
  }

}