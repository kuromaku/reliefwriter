package reliefwriter
import scala.swing._
import scala.swing.event._
import java.io.File
import java.io.FileWriter
import scala.io.Source
import scala.collection.mutable.Map
import javax.swing.border._
import java.awt.Color
import java.awt.Font
import java.awt.GridBagConstraints
import GridBagPanel._
import scala.Enumeration
import scala.xml._

object CodeWriter extends SimpleSwingApplication {
  var left = 'a'
  var right = 'd'
  var down = 's'
  var delete = 'q'
  var nline = 'w'
  var switchCoding = 't'
  var selectCodingArea = 'z'  
  var firstLetter = true
  val debugMode = false
  var bgColor = Color.BLACK
  var fgColor = Color.CYAN
  var txtColor = Color.YELLOW
  val font1 = new Font("Serif",0,12)
  val font2 = new Font("Serif",0,11)
  var switchedArea = false
  
  var mouseClickedAt = new Point(0,0)
  var caretPos = 0
  //font2.
  val cfgFileName = "./config.xml"
  def readConfig : Unit = {
    val f = new File(cfgFileName)
    if (f.exists()) {
      val e = XML.loadFile(f)
      //println("Config file exists.")
      try {
        val bg = (e \\ "BgColor").text.toInt
        bgColor = new Color(bg)
      } catch {
        case _:Throwable => println("Error in reading bgcolor..")
      }
      try {
        val fg = (e \\ "FgColor").text.toInt
      } catch {
        case _:Throwable => println("Error in reading fgcolor..")
      }
      
      try {
        down = (e \\ "GoDown").text.charAt(0)
      } catch {
        case _:Throwable => println("Error in reading the value of GoDown..")
      }
      try {
        left = (e \\ "GoLeft").text.charAt(0)
      } catch {
        case _:Throwable => println("Error in reading the value of GoLeft..")
      }
      try {
        right = (e \\ "GoRight").text.charAt(0)
      } catch {
        case _:Throwable => println("Error in reading the value of GoRight..")
      }    
      try {
        delete = (e \\ "Delete").text.charAt(0)
      } catch {
        case _:Throwable => println("Error in reading the value of Delete..")
      }
      try {
        nline = (e \\ "NLine").text.charAt(0)
      } catch {
        case _:Throwable => println("Error in reading the value of NLine..")
      }    
      try {
        switchCoding = (e \\ "SwitchCoding").text.charAt(0)
      } catch {
        case _:Throwable => println("Error in reading the value of SwitchCoding..")
      }      
      try {
        selectCodingArea = (e \\ "SelectCodingArea").text.charAt(0)
      } catch {
        case _:Throwable => println("Error in reading the value of SelectCodingArea..")
      }
    }
    else {
      println("Config file doesn't exist.")
    }
  }
  def writeConfig : Unit = {
    val fw = new FileWriter(cfgFileName)
    val xmlRep = <Config><BgColor>{bgColor.getRGB}</BgColor><FgColor>{fgColor.getRGB}</FgColor>
    <GoDown>{down}</GoDown><GoLeft>{left}</GoLeft><GoRight>{right}</GoRight>
    <Delete>{delete}</Delete><NLine>{nline}</NLine>
    <SwitchCoding>{switchCoding}</SwitchCoding><SelectCodingArea>{selectCodingArea}</SelectCodingArea></Config>
    scala.xml.XML.write(fw,xmlRep.head,"UTF-8",true,null)
    xmlRep.tail.foreach(e => scala.xml.XML.write(fw,e,"UTF-8",false,null)) 
    fw.close
  }
  val freqfile = new File("./charfreqs.txt")
  
  val tripleTree = if (freqfile.exists) TripleTree.fromCharFreqFile(freqfile) else null
  val tree1ready = freqfile.exists
  
  val evenTree = TripleTree.createEvenTree
  var useEven = true
  //evenTree.printCodes
  if (debugMode) {
    println("Size 1: "+tripleTree.size)
    println("Size 2: "+evenTree.size)
  }  
  //var current:CodeTree = ct
  var tripleCurrent = if (useEven || !tree1ready) evenTree else tripleTree
  object tfield extends TextArea
  
  val textArea = new TextArea {
    editable_=(false)
  }
  
  val maxCharsInAField = 50
  def lt2(s1:String,s2:String) : Boolean = {
    s1.charAt(s1.length-1) < s2.charAt(s2.length-1)
  }
  object field extends TextArea("Type code into this box") {
    maximumSize_=(new Dimension(100,45))
    minimumSize_=(new Dimension(60,30))
    preferredSize_=(new Dimension(75,45))
    //border_=(new TitledBorder("TypingArea"))
    border_=(new TitledBorder(new LineBorder(fgColor,5),"TypingArea",0,0,font2,new Color(111,111,111)))
    background_=(bgColor)
    foreground_=(fgColor)
  }
  object area extends TextArea("Typed text will appear here.") {
    maximumSize_=(new Dimension(1280,1024))
    minimumSize_=(new Dimension(300,200))
    preferredSize_=(new Dimension(640,480))
    
    border_=(new TitledBorder(new LineBorder(fgColor,5),"DisplayArea",0,0,font1,new Color(111,111,111)))
    background_=(bgColor)
    foreground_=(fgColor)
  }
  object scroller extends ScrollPane(area) {
    //preferredSize_=(new Dimension(650,490)
    val sizeMin = area.size
    minimumSize_=(new Dimension(sizeMin.getWidth.toInt+10,sizeMin.getHeight.toInt+10))
  }
  
  object panel2 extends BorderPanel {
    border_=(new LineBorder(Color.BLACK))
    background_=(bgColor)
    //import BorderPanel._
    layout(area) = BorderPanel.Position.Center
    layout(field) = BorderPanel.Position.South
  }
  object panel extends GridBagPanel {
    //val d = new Dimension
    //d.setSize(640,480)
    border_=(new LineBorder(Color.BLACK))
    background_=(bgColor)
    val c = new Constraints
    c.fill = Fill.Horizontal
    c.gridx = 0
    c.gridy = 0
    c.weightx_=(1)
    c.weighty_=(1)
    c.gridheight = 3
    layout(scroller) = c
    
    val c2 = new Constraints
    c2.gridx = 0
    c2.gridy = 3
    c2.ipady = 5
    c2.ipadx = 5
    c2.weightx_=(0.9)
    c2.weighty_=(0.9)
    layout(field) = c2
  }
  def top = new MainFrame {
    title_=("ReliefWriter")
    location_=(new Point(300,200))
    val d = new Dimension
    d.setSize(800, 600)
    preferredSize_=(d)
    
    readConfig
    
    object saveAs extends MenuItem("Save As") {
      object saveAction extends Action("Save") {
        def apply : Unit = {
          val fc = new FileChooser
          val response = fc.showSaveDialog(saveAs)
          if (response.toString.equals("Approve")) {
            val fw = new FileWriter(fc.selectedFile)
            fw.write(area.text)
            fw.close
          }
        }
      }
      action_=(saveAction)
    }
    object loadFile extends MenuItem("Load File") {
      object loadAction extends Action("Load") {
        def apply : Unit = {
          val fc = new FileChooser
          val response = fc.showOpenDialog(loadFile)
          
          if (response.toString.equals("Approve")) {
            area.text_=("")
            firstLetter = false
            for (line <- Source.fromFile(fc.selectedFile).getLines) {
              area.append(line+"\n")
            }
          }
        }
      }
      action_=(loadAction)
    }
    object writeConfigNow extends MenuItem("Write Config") {
      object configWrite extends Action("Write Configuration") {
        def apply : Unit = {
          writeConfig
        }
      }
      action_=(configWrite)
    }
    def addSpace(n:Int) : String = {
      var ss = " "
      for (i <- 1 until n) {
        ss = ss+" "
      }
      ss
    }
    object showCodes extends MenuItem("Show Codes") {
      object showAction extends Action("Show Codes") {
        def apply : Unit = {
          val codeFrame = new Frame
          val minL = 16
          codeFrame.minimumSize_=(new Dimension(200,300))
          codeFrame.location_=(new Point(50,50))
          val codeArea = new TextArea
          codeArea.background_=(bgColor)
          codeArea.foreground_=(fgColor)
          var codeLists = if (useEven || !tree1ready) evenTree.buildCodeStrings(""+left,""+down,""+right) else tripleTree.buildCodeStrings(""+left,""+down,""+right)
          codeLists = codeLists.sortWith(lt2)
          codeArea.append("The "+codeLists.size+" codes in use are:\n\n")
          //codeArea.preferredSize_=(new Dimension(120,650))
          codeArea.minimumSize_=(new Dimension(190,300))

          var evenLine = false
          for (cs <- codeLists) {
            if (evenLine) {
              codeArea.append(cs+"\n")
            }
            else {
              codeArea.append(cs+addSpace(minL-cs.length))
            }
            evenLine = !evenLine
          }
          codeArea.enabled_=(false)
          val scroller2 = new ScrollPane(codeArea)
          val flowPane = new FlowPanel(scroller2)
          flowPane.background_=(bgColor)
          codeFrame.contents_=(flowPane)
          codeFrame.pack
          codeFrame.visible_=(true)
          
          
        }
      }
      action_=(showAction)
    }
    val mBar = new MenuBar {
      background_=(bgColor)
      //border_=(new TitledBorder("Menu"))
      contents += new Menu("File") {
        foreground_=(fgColor)
        contents += loadFile
        contents += new Separator
        contents += saveAs
        contents += new Separator
        contents += writeConfigNow
      }
      contents += new Menu("Help") {
        contents += showCodes
        foreground_=(fgColor)
      }
    }
    menuBar_=(mBar)
    contents = panel2
    reactions +=  {
      case KeyTyped(`field`,x,_,_) => {
        if (firstLetter) {
          firstLetter = false
          area.text_=("")
          field.text_=("")
        }
        if (switchedArea) {
          area.text_=(area.text.dropRight(1))
          switchedArea = false
        }
        var update = false
        if (x == switchCoding) {
          useEven = !useEven
          if (useEven || !tree1ready) tripleCurrent = evenTree else tripleCurrent = tripleTree
          if (debugMode) {
            println("Switching coding scheme...")
          }
        }
        if (x == left) {
          tripleCurrent = tripleCurrent.goLeft
          update = true
        }
        else if (x == right) {
          tripleCurrent = tripleCurrent.goRight
          update = true
        }
        else if (x == down) {
          tripleCurrent = tripleCurrent.goDown
          update = true
        }
        else if (x == delete) {
          if (caretPos >= (area.text.length-1)) {
            area.text_=(area.text.dropRight(1))
          }
          else {
            area.text_=(area.text.substring(0,caretPos-1)+area.text.substring(caretPos))
          }
          caretPos -= 1
          if (caretPos < 1) {
            caretPos = 1
          }
        }
        else if (x == nline) {
          if (caretPos >= (area.text.length-1)) {
            area.append("\n")
          }
          else {
            area.text_=(area.text.substring(0,caretPos)+"\n"+area.text.substring(caretPos))
          }
          caretPos += 1
        }
        if (update) {
          val c = tripleCurrent.getCode
          if (c.length() == 1) {
            if (caretPos >= (area.text.length-1)) {
              area.append(c)
            }
            else {
              area.text_=(area.text.substring(0,caretPos)+c+area.text.substring(caretPos))
            }
            field.append("|")
            tripleCurrent = if (useEven || !tree1ready) evenTree else tripleTree
            caretPos += 1
          }
          else {
            
          }
          if (field.text.length() > maxCharsInAField) field.text_=("")
        update = false
        }
        
      }
      case KeyTyped(`area`,x,_,_) => {
        if (x == selectCodingArea) {
          switchedArea = true
          area.peer.transferFocus() //the idea is that the next component will be the field
          if (caretPos < (area.text.length-1)) {
            area.text_=(area.text.substring(0,caretPos)+area.text.substring(caretPos))
          }
        }
      }
      case MouseClicked(`area`,point,mods,clicks2,_) => {
        caretPos = area.caret.position
        tripleCurrent = if (useEven || !tree1ready) evenTree else tripleTree //resets the tree
        mouseClickedAt = point
      }
      case _ => Unit
    }
    listenTo(field.keys,area.mouse.clicks,area.keys)
    

  }
}