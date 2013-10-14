package HuffmanEnCoding

object Huffman {

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match{    
    case Leaf(char,_weight) => _weight
    case Fork(left,right,_chars,_weight) => weight(left) + weight(right)
  }

  def chars(tree: CodeTree): List[Char] = tree match{
    case Leaf(char,_weight) => List[Char](char)
    case Fork(left,right,_chars,_weight) => chars(left) ::: chars(right)    
  } 

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] =   chars
  														                        .sortWith(_<_)
                        														  .groupBy(x =>x)
                        														  .map(x => (x._1,x._2.length))
                        														  .toList
  														  
  
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.map(x => new Leaf(x._1,x._2)) sortBy{ case Leaf(_char,_weight) => _weight}

  
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1
  
  def combine(trees: List[CodeTree]): List[CodeTree] = {
		if(trees.length < 2) trees
		else new Fork(trees(0),trees(1),chars(trees(0)):::chars(trees(1)),weight(trees(0))+weight(trees(1))) :: trees.drop(2) sortBy{	case Fork(left,right,_chars,_weight) => weight(left) + weight(right)
                                                              																																	 	case Leaf(_chars,_weight) => _weight	
                                                              																																  }
	}                      
 
  def until(single_function: List[CodeTree]=>Boolean, combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]):CodeTree = 
    if(single_function(trees)) trees(0)
    else until(single_function,combine)(combine(trees))

  def createCodeTree(chars: List[Char]): CodeTree = until(singleton,combine)(makeOrderedLeafList(times(chars)))

  type Bit = Int

  def decode(tree:CodeTree,bits:List[Bit]):List[Char] = {	
		def decode_helper(tree_helper:CodeTree, bits_helper:List[Bit],acc:List[Char]):List[Char] = {
			bits_helper match {					

			case Nil => tree_helper match {
				case Leaf(_char,_weight) => acc :+ _char
				case Fork(left,right,_char,_weight) => throw new Error("There is a problem in the encoding")
			  }

			case x::xs => tree_helper match {			
			 case Leaf(_char,_weight) => decode_helper(tree,bits_helper,acc :+ _char)
			 case Fork(left,right,_char,_weight) => decode_helper(if(x == 0)left else right,bits_helper.tail,acc)			
			 }
			
			}
		}		
		decode_helper(tree,bits,List[Char]())	
	}         
  
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
 
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
 
  def decodedSecret: List[Char] = decode(frenchCode,secret)

  def encode(tree:CodeTree)(text:List[Char]):List[Bit] = {
  
  	def encode_helper(tree_help:CodeTree,text_helper:List[Char],acc:List[Bit]):List[Bit] = {
  	
  					text_helper match {  					
  						case Nil => acc
  					
  						case x :: tail => tree_help match {	
  							   case Leaf(_char,_weight) => encode_helper(tree,tail,acc)
  							   case Fork(left,right,_chars,_weight) => if(chars(left) contains x) encode_helper(left,text_helper, acc :+ 0) else encode_helper(right,text_helper,acc:+ 1)	
  							 }
  					}
  	}
  encode_helper(tree,text,List())
  } 

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = {  
   table match{
     case Nil => throw new Error("There is no character in the table")
     case x::tail => if(x._1 == char) x._2 else codeBits(tail)(char)
   }
 }


  def convert(tree: CodeTree): CodeTable = { 
 		def convert_helper(tree_helper:CodeTree,acc:CodeTable):CodeTable = {
 				tree_helper match {
 					case Leaf(_char,_weight) => acc ::: List((_char,encode(tree)(List(_char)))) 				
 					case Fork(left,right,_chars,_weight) => convert_Fork(tree_helper,acc,_chars)
 					}
 			}
 	
 		def convert_Fork(_tree:CodeTree,acc:CodeTable,_chars:List[Char]):CodeTable = {
 			_chars match {
 				case Nil => acc
		 		case x:: tail => convert_Fork(_tree,acc ::: List((x,encode(tree)(List(x)))),tail)
 			}	
 		}
 		convert_helper(tree,List[(Char,List[Int])]())
 	}        

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def quickEncodeHelp(code_table:CodeTable)(text_help:List[Char],acc:List[Bit]):List[Bit] = {
      text_help match {
        case Nil => acc
        case x:: tail => quickEncodeHelp(code_table)(tail,acc ::: codeBits(code_table)(x))
      }
    }    
    quickEncodeHelp(convert(tree))(text,List())
  }
}


object Main{

  def main(args:Array[String]) = {

    //creating the huffman tree
    val huffmanTree = Huffman.createCodeTree(List('a','b','c','a','d'))
    println("Huffman tree: "+ huffmanTree)
    //encoding a text using the huffman tree
    val encoding = Huffman.quickEncode(huffmanTree)("abcdda".toList)

    println(encoding.toString)
  }

}