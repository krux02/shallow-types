
trait ShallowType {
	def size:Int
	def name:String
}

sealed trait PrimitiveType extends ShallowType {
	def size:Int
	def name:String
}

case object PFloat  extends PrimitiveType { val size = 4; val name = "Float"  }
case object PDouble extends PrimitiveType { val size = 8; val name = "Double" }
case object PChar   extends PrimitiveType { val size = 2; val name = "Char"   }
case object PByte   extends PrimitiveType { val size = 1; val name = "Byte"   }
case object PShort  extends PrimitiveType { val size = 2; val name = "Short"  }
case object PInt    extends PrimitiveType { val size = 4; val name = "Int"    }
case object PLong   extends PrimitiveType { val size = 8; val name = "Long"   }

case class Attribute(name:String, tType:ShallowType) {
	def getter(offset:Int):String = {
		tType match {
		case p:PrimitiveType =>
			val typeName = if(p == PByte) "" else p.name
			"\tdef %s = buffer.get%s(offset+%d)\n".format(name, typeName, offset)
		case c:ComposedType =>
			"\tdef %s = new %s(buffer, offset+%d)\n".format(name, c.name, offset)
		}
	}
	
	def setter(offset:Int):String = {
		tType match {
		case p:PrimitiveType =>
			val typeName = if(p == PByte) "" else p.name
			s"\tdef ${name}_=(_${name}:Byte) { buffer.put${typeName}(offset+${offset}, _${name}) }\n"
		case c:ComposedType =>
s"""
	def ${name}_=(_${name}:Position) {
		val bufferHere  = buffer.duplicate
		val bufferThere = _${name}.buffer.duplicate
		
		bufferThere.position(_${name}.offset)
		bufferThere.limit(_${name}.offset+${offset})
		
		bufferHere.position(offset)
		bufferHere put bufferThere
	}
"""
		}
	}
}

case class ComposedType(name:String, attributes:Vector[Attribute], indexable:Boolean = false) extends ShallowType {
	val size = attributes.map( _.tType.size ).sum
	def classHead = s"class ${name}(val buffer:ByteBuffer, val offset:Int) extends Shallow[${name}] {\n"
	def constructor(sb:StringBuilder) = {
		sb ++= "\tdef this("
		sb ++= attributes.map(a => a.name + ": " + a.tType.name).mkString(", ")
		sb ++= ") = {\n"
		
		sb ++= "\t\tthis(ByteBuffer.allocate("+size+"),0)\n"
		
		for(a <- attributes) a.tType match {
		case ct:ComposedType =>
			sb ++= s"\t\tval ${a.name}Buffer = pos.buffer.duplicate\n"
			sb ++= s"\t\t${a.name}Buffer.position(${a.name}.offset)\n"
			sb ++= s"\t\t${a.name}Buffer.limit(${a.name}.offset+${ct.size})\n"
		case _ =>
		}
		
		sb ++= "\t\tbuffer."
		
		for( a <- attributes ) {
			if(a.tType == PByte) 
				sb ++= "put("+a.name+")" 
			else if(a.tType.isInstanceOf[PrimitiveType])
				sb ++= "put"+a.tType.name+"("+a.name+")"
			else
				sb ++= "put("+a.name+"Buffer)"
			
			sb += '.'
		}
		sb ++= "rewind\n\t}\n"
	}
	
	def stride  = "\tdef stride = %d\n".format(size)
	def require = "\trequire(buffer.position == 0, \"buffer position not 0\")\n"
	def copy    = s"""
	def copy = {
		val bufferHere = ByteBuffer.allocate(${size})
		val bufferThere = buffer.duplicate
		bufferThere.position(offset).limit(offset+${size})
		bufferHere put bufferThere
		bufferHere.rewind
		new ${name}(bufferHere,0)
	}\n"""
	
	def toStringCode:String = {
		"\toverride def toString = " +
		attributes.map(a => a.tType match { 
			case PFloat | PDouble               => "%4.2f"
			case PByte  | PShort | PInt | PLong => "%d"
			case PChar                          => "%c"
			case _                              => "%s"}).mkString("\""+name+"(",", ", ")\".format")+attributes.map(_.name).mkString("(",",",")\n")
	}
	
	def indexCode:String = {
		"\tdef index = offset / "+size
	}
	
	def generateCode(sb:StringBuilder) = {
		sb ++= classHead
		constructor(sb)
		sb ++= stride
		sb ++= require
		
		var offset = 0
		for(a <- attributes) {
			sb ++= a.getter(offset)
			sb ++= a.setter(offset)
			offset += a.tType.size
		}
		
		if(indexable)
			sb ++= indexCode
		
		sb ++= copy
		sb ++= toStringCode
		sb ++= "}\n"
		sb
	}
}

object Input {
	val position = ComposedType( "Position", Vector( Attribute("x", PFloat), Attribute("y", PFloat), Attribute("z", PFloat) ) )
	val color    = ComposedType( "Color"   , Vector( Attribute("a", PByte), Attribute("r", PByte), Attribute("g", PByte), Attribute("b", PByte) ) )
	val vertex   = ComposedType( "Vertex"  , Vector( Attribute("pos", position), Attribute("col", color) ) )
}

// Code generation Pieces

object CodeGenerator {
	def printAllInput{
		println(genCode(Input.position))
		println(genCode(Input.color))
		println(genCode(Input.vertex))
	}
	
	def generateCode {
		val writer = new java.io.FileWriter("gen")
		import writer.{append,close}
		append("package gen\n\n")
		append(genCode(Input.position))
		append(genCode(Input.color))
		append(genCode(Input.vertex))
		close
	}
	
	def genCode(tType:ComposedType) = {
		val sb = new StringBuilder
		tType.generateCode(sb)
		sb.result
	}
}

