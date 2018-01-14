package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(
      Fork(
        Leaf('a',2), Leaf('b',3), List('a','b'), 5),
      Leaf('d',4), List('a','b','d'), 9)
	}

  trait CodeTables {
    val ct1 = List(
      ('a', List(0,0)),
      ('b', List(0,1)),
      ('c', List(1,0)),
      ('d', List(1,1)))
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    assert(times(string2Chars("hello, world")) === List(('e',1), (' ',1), (',',1), ('l',3), ('h',1), ('r',1), ('w',1), ('o',2), ('d',1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton true") {
    assert(singleton(List(Leaf('a', 1))) === true)
  }

  test("singleton false") {
    assert(singleton(List(Leaf('a', 1), Leaf('b', 2))) === false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of leaf list where elements get moved around according to their weights") {
    val leaflist = List(Leaf('a', 2), Leaf('b', 1), Leaf('c', 2))
    assert(combine(leaflist) === List(Fork(Leaf('b',1),Leaf('a',2),List('b', 'a'),3), Leaf('c',2)))
    assert(combine(combine(leaflist)) === List(Fork(Leaf('c',2),Fork(Leaf('b',1),Leaf('a',2),List('b', 'a'),3),List('c', 'b', 'a'),5)))
  }

  test("combine of leaf list with 1 element") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) == leaflist)
  }

  test("create code tree") {
    //assert(createCodeTree(string2Chars("hello, world!")) === Fork(Fork(Fork(Fork(Fork(Fork(Fork(Fork(Fork(Leaf('e',1),Leaf('!',1),List('e', '!'),2),Leaf(' ',1),List('e', '!', ' '),3),Leaf(',',1),List('e', '!', ' ', ','),4),Leaf('h',1),List('e', '!', ' ', ',', 'h'),5),Leaf('r',1),List('e', '!', ' ', ',', 'h', 'r'),6),Leaf('w',1),List('e', '!', ' ', ',', 'h', 'r', 'w'),7),Leaf('d',1),List('e', '!', ' ', ',', 'h', 'r', 'w', 'd'),8),Leaf('o',2),List('e', '!', ' ', ',', 'h', 'r', 'w', 'd', 'o'),10),Leaf('l',3),List('e', '!', ' ', ',', 'h', 'r', 'w', 'd', 'o', 'l'),13))
    println(createCodeTree(string2Chars("hello, world!")))
  }

  test("decoded secret") {
    assert(decodedSecret.mkString === "huffmanestcool")
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("code bits basic functionality") {
    new CodeTables {
      assert(codeBits(ct1)('c') === List(1, 0))
    }
  }

  test("convert CodeTree to CodeTable basic functionality") {
    new TestTrees {
      assert(convert(t2) === List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
    }
  }

  test("quickEncode basic functionality") {
    new TestTrees {
      assert(quickEncode(t2)(List('a', 'a', 'b', 'b', 'd')) === List(0, 0, 0, 0, 0, 1, 0, 1, 1))
    }
  }
}
