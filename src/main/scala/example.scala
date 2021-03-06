

// input data

/* 
@Vertex
case class Position(x:Float,y:Float,z:Float)
@Vertex
case class Color(a:Byte,r:Byte,g:Byte,b:Byte)
@Vertex
case class Vertex(pos:Position,col:Color)

case class Triangle(v1:Vertex,v2:Vertex,v3:Vertex)
type TriangleMesh = IndexedSeq[Triangle]
*/

// generated

import java.nio.{ByteBuffer, IntBuffer}

/*
sealed trait Color {
	def a:Byte
	def a_=(_a:Byte)
	def r:Byte
	def r_=(_r:Byte)
	def g:Byte
	def g_=(_g:Byte)
	def b:Byte
	def b_=(_b:Byte)
}

sealed trait Position {
	def x:Float
	def x_=(_x:Float)
	def y:Float
	def y_=(_y:Float)
	def z:Float
	def z_=(_z:Float)
}

sealed trait Vertex {
	def pos:Position
	def pos_=(_pos:Position)
	def col:Color
	def col_=(_col:Color)
}

sealed trait Triangle {
	def v1:Vertex
	def v1_=(_v1:Vertex)
	def v2:Vertex
	def v2_=(_v2:Vertex)
	def v3:Vertex
	def v3_=(_v3:Vertex)
}
*/


trait Shallow[T] {
	def stride:Int
	def copy:T
}

class Color(val buffer:ByteBuffer, val offset:Int) extends Shallow[Color] {
	def this(a:Byte, r:Byte, g:Byte, b:Byte) = {
		this(ByteBuffer.allocate(4),0)
		buffer.put(a).put(r).put(g).put(b).rewind
	}
	
	require(buffer.position == 0, "buffer position not 0")
	
	def a = buffer.get(offset  )
	def r = buffer.get(offset+1)
	def g = buffer.get(offset+2)
	def b = buffer.get(offset+3)
	def stride = 4
	
	def a_=(_a:Byte) { buffer.put(offset  ,_a) } 
	def r_=(_r:Byte) { buffer.put(offset+1,_r) }
	def g_=(_g:Byte) { buffer.put(offset+2,_g) } 
	def b_=(_b:Byte) { buffer.put(offset+3,_b) }
	
	def copy = {
		val bufferHere = ByteBuffer.allocate(4)
		val bufferThere = buffer.duplicate
		bufferThere.position(offset).limit(offset+4)
		bufferHere put bufferThere
		bufferHere.rewind
		new Color(bufferHere,0)
	}
	
	override def toString = "Color(%d, %d, %d, %d)".format(a,r,g,b)
}

class Position(val buffer:ByteBuffer, val offset:Int) extends Shallow[Position] {
	def this(x:Float,y:Float,z:Float) = {
		this(ByteBuffer.allocate(12),0)
		buffer.putFloat(x).putFloat(y).putFloat(z).rewind
	}
	
	require(buffer.position == 0, "buffer position not 0")
	
	def x = buffer.getFloat(offset  )
	def y = buffer.getFloat(offset+4)
	def z = buffer.getFloat(offset+8)
	def stride = 12
	
	def x_=(_x:Float) { buffer.putFloat(offset+0,_x) }
	def y_=(_y:Float) { buffer.putFloat(offset+4,_y) }
	def z_=(_z:Float) { buffer.putFloat(offset+8,_z) }
	
	def +=(that:Position) = {
		x += that.x
		y += that.y
		z += that.z
		this
	}
	
	def +(that:Position) = copy += that
	
	def copy = {
		val bufferHere = ByteBuffer.allocate(12)
		val bufferThere = buffer.duplicate
		bufferThere.position(offset).limit(offset+12)
		bufferHere put bufferThere
		bufferHere.rewind
		new Position(bufferHere,0)
	}
	
	override def toString = "Position(%2.2f, %2.2f, %2.2f)".format(x,y,z)
}

class Vertex(val buffer:ByteBuffer, val offset:Int) extends Shallow[Vertex] {
	def this(pos:Position, col:Color) = {
		this(ByteBuffer.allocate(16),0)
		val posBuffer = pos.buffer.duplicate
		posBuffer.position(pos.offset)
		posBuffer.limit(pos.offset+12)
		val colBuffer = col.buffer.duplicate
		colBuffer.position(pos.offset)
		colBuffer.limit(pos.offset+4)
		buffer.put(posBuffer).put(colBuffer).rewind
	}
	
	require(buffer.position == 0, "buffer position not 0")
	
	def pos   = new Position(buffer, offset   )
	def col   = new Color   (buffer, offset+12)
	def stride     = 16
	lazy val index = offset/stride
	
	def pos_=(_pos:Position) {
		val bufferHere  = buffer.duplicate
		val bufferThere = _pos.buffer.duplicate
		
		bufferThere.position(_pos.offset)
		bufferThere.limit(_pos.offset+12)
		
		bufferHere.position(offset)
		bufferHere put bufferThere
	}
	
	def col_=(_col:Color) {
		val bufferHere  = buffer.duplicate
		val bufferThere = _col.buffer.duplicate
		
		bufferThere.position(_col.offset)
		bufferThere.limit(_col.offset+4)
		
		bufferHere.position(offset+12)
		bufferHere put bufferThere
	}
	
	def copy = {
		val bufferHere = ByteBuffer.allocate(16)
		val bufferThere = buffer.duplicate
		bufferThere.position(offset).limit(offset+16)
		bufferHere put bufferThere
		bufferHere.rewind
		new Vertex(bufferHere,0)
	}
	
	override def toString = "Vertex(%s, %s)".format(pos, col)
}

class Triangle(var v1:Vertex,var v2:Vertex,var v3:Vertex) {
	require( (v1.buffer eq v2.buffer) && (v2.buffer eq v3.buffer) )
}

class VertexBuffer(val buffer:ByteBuffer) extends IndexedSeq[Vertex] {
	def this(length:Int, direct:Boolean = true) = 
	    this( if(direct) ByteBuffer.allocateDirect(length*16) else ByteBuffer.allocate(length*16) )

	def apply(idx: Int) = new Vertex(buffer,idx*16)
	
	def length = buffer.capacity / 16
	
	def update(idx: Int, elem: Vertex) {
		val bufferHere  = buffer.duplicate
		val bufferThere = elem.buffer.duplicate
		
		bufferThere.position(elem.offset)
		bufferThere.limit(elem.offset + 16)
		
		bufferHere.position(idx*16)
		bufferHere put bufferThere
	}
	
	def copy = {
		new VertexBuffer(ByteBuffer.allocate(buffer.capacity).put(buffer))
	}
}

class TriangleMesh(val vertexIndices:IntBuffer, vertices:VertexBuffer) extends IndexedSeq[Triangle] {
	def apply(idx: Int) = {
		val v1 = vertices(vertexIndices.get(idx*3  ))
		val v2 = vertices(vertexIndices.get(idx*3+1))
		val v3 = vertices(vertexIndices.get(idx*3+2))
		new Triangle(v1,v2,v3)
	}
	
	def length: Int     = vertexIndices.capacity / 3
	
	def update(idx: Int, elem: Triangle) {
		require(idx < length,"index out of bounds " + idx + " " + length)
		require(vertices.buffer eq elem.v1.buffer, "Triangles need to be from the same Buffer")
		
		val index1 = elem.v1.index
		val index2 = elem.v2.index
		val index3 = elem.v3.index
		
		vertexIndices.put(idx*3  , index1)
		vertexIndices.put(idx*3+1, index2)
		vertexIndices.put(idx*3+2, index3)
	}
}
