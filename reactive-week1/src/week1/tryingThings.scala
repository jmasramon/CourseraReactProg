import java.util.Random

val rand = new Random

println(rand.nextInt())

trait Generator[+T] {
	self =>
	
	def generate: T
	
	def map[B](f: T => B): Generator[B] = new Generator[B] {
		def generate = f(self.generate)
	}

	def flatMap[B](f: T => Generator[B]): Generator[B] = new Generator[B] {
		def generate = f(self.generate).generate
	}
}

val integers = new Generator[Int] {
	def generate:Int = rand.nextInt()
}

println(integers.generate)

val booleans = new Generator[Boolean] {
	def generate: Boolean = integers.generate > 0
}

println(booleans.generate)

val pairs = new Generator[(Int,Int)] {
	def generate = (integers.generate, integers.generate)
}

println(pairs.generate)

val booleans2 = for (x <- integers) yield x>0

println(booleans2.generate)

def pairs[T,U](t: Generator[T], u:Generator[U]) = for {
	x <- t
	y <- u
} yield (x,y)

println(pairs(integers, booleans).generate)

def single[T](x: T): Generator[T] = new Generator[T]  {
	def generate = x
}

println(single(3).generate)



trait Tree
case class Inner(left:Tree, right:Tree) extends Tree
case class Leaf(x: Int) extends Tree 

def trees[Tree]: Generator[Tree] = for {
	isLeaf <- booleans
	tree <- if(isLeaf) leaf else inner 
} yield tree

def leaf = Leaf(integers.generate)

def inner: Inner = for {
	leftIsLeaf <- booleans
	left <- if(leftIsLeaf) leaf else inner
	rightIsLeaf <- booleans
	right <- if(rightIsLeaf) leaf else inner
} yield Inner(left, right)