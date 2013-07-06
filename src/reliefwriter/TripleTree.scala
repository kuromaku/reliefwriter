package reliefwriter
import java.io.File
import scala.io.Source

class TripleTree(code:String,p:Double,t1:TripleTree,t2:TripleTree,t3:TripleTree) {
  def getCode = code
  def getProbability = p
  
  def goLeft = t1
  def goDown = t2
  def goRight = t3
  
  def printBranch(s:String) : Unit = {
    if (code.length == 1) {
      println("Code: "+s+" is "+code+" (prob: "+p.toString+")")
    }
    if (t1 != null) {
      t1.printBranch(s+"0")
    }
    if (t2 != null) {
      t2.printBranch(s+"1")
    }
    if (t3 != null) {
      t3.printBranch(s+"2")
    }
  }
  def printCodes : Unit = {
    t1.printBranch("0")
    t2.printBranch("1")
    t3.printBranch("2")
  }
  def buildCodeStrings(c1:String,c2:String,c3:String) : List[String] = {
    t1.buildStrings(c1,c1,c2,c3) ++ t2.buildStrings(c2,c1,c2,c3) ++ t3.buildStrings(c3,c1,c2,c3)
  }
  def buildStrings(s:String,c1:String,c2:String,c3:String) : List[String] = {
    if (code.length() == 1) {
      List[String](s+" is "+code)
    }
    else {
    val l1 = if (t1 != null) t1.buildStrings(s+c1,c1,c2,c3) else List[String]()
    val l2 = if (t2 != null) t2.buildStrings(s+c2,c1,c2,c3) else List[String]()
    val l3 = if (t3 != null) t3.buildStrings(s+c3,c1,c2,c3) else List[String]()
    
    if (l1.size > 0) {
      if (l2.size > 0) { 
        l1 ++ l2 ++ l3
      }
      else l3
    }
    else {
      if (l2.size > 0)
        l2 ++ l3
        else l3
    }
    }
  }
  def size : Int = {
    if (t1 == null && t2 == null && t3 == null) {
      1
    }
    else {
      val s1 = if (t1 != null) t1.size else 0
      val s2 = if (t2 != null) t2.size else 0
      val s3 = if (t3 != null) t3.size else 0
      s1 + s2 + s3
    }
  }
}
object TripleTree {
  val debugMode = false
  def createEvenTree : TripleTree = {
    var l = List[(String,Double)]()
    val p = 1.0/(127-32)
    for (i <- 32 until 127) {
      l = l.+:((""+i.toChar,p))
    }
    createTripleTree(l)
  }
  def createTripleTree(list:List[(String,Double)]) : TripleTree = {
    var tl = List[TripleTree]()
    var set2 = list.toSet
    
    while (set2.size >= 3) {
      val a = set2.toArray sortWith(_._2 < _._2)
      val (c1,p1) = a(0)
      val (c2,p2) = a(1)
      val (c3,p3) = a(2)
      if (c1.length == 1 && c2.length == 1 && c3.length == 1) {
        tl = tl.+:((new TripleTree(c1+c2+c3,p1+p2+p3,leaf(c1,p1),leaf(c2,p2),leaf(c3,p3))))
      }
      else if (c1.length == 1 && c2.length == 1) {
        val t3b = tl.find(_.getCode == c3)
        tl = tl.filter(_.getCode != c3)
        tl = tl.+:((new TripleTree(c1+c2+c3,p1+p2+p3,leaf(c1,p1),leaf(c2,p2),t3b.get)))
      }
      else if (c1.length == 1 && c3.length == 1) {
        val t2b = tl.find(_.getCode == c2)
        tl = tl.filter(_.getCode != c2)
        tl = tl.+:((new TripleTree(c1+c2+c3,p1+p2+p3,leaf(c1,p1),t2b.get,leaf(c3,p3))))
      }  
       else if (c2.length == 1 && c3.length == 1) {
        val t1b = tl.find(_.getCode == c1)
        tl = tl.filter(_.getCode != c1)
        tl = tl.+:((new TripleTree(c1+c2+c3,p1+p2+p3,t1b.get,leaf(c2,p2),leaf(c3,p3))))
      }
       else if (c3.length == 1) {
         val t1b = tl.find(_.getCode == c1)
         val t2b = tl.find(_.getCode == c2)
         tl = tl.filter(_.getCode != c1)
         tl = tl.filter(_.getCode != c2)
         tl = tl.+:((new TripleTree(c1+c2+c3,p1+p2+p3,t1b.get,t2b.get,leaf(c3,p3))))
       }
       else if (c2.length == 1) {
         val t1b = tl.find(_.getCode == c1)
         val t3b = tl.find(_.getCode == c3)
         tl = tl.filter(_.getCode != c1)
         tl = tl.filter(_.getCode != c3)
         tl = tl.+:((new TripleTree(c1+c2+c3,p1+p2+p3,t1b.get,leaf(c2,p2),t3b.get)))
       }
       else if (c1.length == 1) {
         val t2b = tl.find(_.getCode == c2)
         val t3b = tl.find(_.getCode == c3)
         tl = tl.filter(_.getCode != c2)
         tl = tl.filter(_.getCode != c3)
         tl = tl.+:((new TripleTree(c1+c2+c3,p1+p2+p3,leaf(c1,p1),t2b.get,t3b.get)))
       }
       else {
         val t1b = tl.find(_.getCode == c1)
         val t2b = tl.find(_.getCode == c2)
         val t3b = tl.find(_.getCode == c3)
         tl = tl.filter(_.getCode != c1)
         tl = tl.filter(_.getCode != c2)
         tl = tl.filter(_.getCode != c3)
         tl = tl.+:((new TripleTree(c1+c2+c3,p1+p2+p3,t1b.get,t2b.get,t3b.get)))         
       }
      set2 = a.drop(3).toSet + ((c1+c2+c3,p1+p2+p3))
    }
    if (tl.size == 1) {
      if (debugMode) { 
        println("No error.. set2 size: "+set2.size)
        if (set2.size > 0) println(set2.head)  
      }
      tl.head
    }
    else { //tl.size should be 2
      val th1 = tl.head
      val th2 = tl.tail.head
      if (set2.size == 1) {
        val (s,p) = set2.head
        val th3 = leaf(s,p)
        new TripleTree(th1.getCode+th2.getCode+s,th1.getProbability+th2.getProbability+p,th1,th2,th3)
      }
      else {
        //something should be done to correct this...
        val px1 = th1.getProbability
        val px2 = th2.getProbability
        val (s1,px3) = set2.head
        val (s2,px4) = set2.tail.head
        
        if (debugMode) {
          println("error error..."+set2.size)
          var pa = new Array[(String,Double)](4)
          pa(0) = (th1.getCode,px1)
          pa(1) = (th2.getCode,px2)
          pa(2) = (s1,px3)
          pa(3) = (s2,px4)
          for (i <- 0 until (pa.length-1)) {
            for (j <- i+1 until pa.length) {
              if (pa(i)._1 == pa(j)._1) {
                println("Identical pair: ")
                println(pa(i))
                println(pa(j))
              }
            }
          }
          pa = pa.sortWith(_._2 < _._2)
        }
        new TripleTree(th1.getCode+th2.getCode,th1.getProbability+th2.getProbability,th1,th2,null)
      }
    }
  }
  def leaf(s:String,p:Double) : TripleTree = { new TripleTree(s,p,null,null,null) }
  
  def fromCharFreqFile(f:File) : TripleTree = {
    var l = List[(Char,Int)]()
    var sum = 0
    val minCount = 500
    var fullset = Set[Char]()
    for (i <- 32 until 127) {
      val c = i.toChar
      fullset = fullset + c
    }
    for (line <- Source.fromFile(f).getLines) {
      val a = line.split(' ')
      if (a.length >= 2) {
        val c = a(0).toInt.toChar
        val count = a(1).toInt
        
        val aux = if (count > minCount) { count } else { minCount }
        sum += aux
        l = l.+:((c,aux))
        if (c >= 32.toChar) {
          fullset = fullset - c
        }
      }
    }
    for (c <- fullset) {
      l = l.+:((c,minCount))
      sum += minCount
    }
    val fa = new Array[(String,Double)](l.size)
    var idx = 0
    for ((c,num) <- l) {
      fa(idx) = (c.toString,1.0*num/sum)
      idx += 1
    }
    createTripleTree(fa.toList)
  }  
}