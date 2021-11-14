@main def main: Unit =
  object ex11 extends org.scalatest.FunSuite {

    // A Parser[A] takes a string and a starting index
    // and tries to parse a substring of the string beginning at
    // that starting index. If it fails, it returns None. If it
    // succeeds, it returns Some containing a tuple of a result of
    // type A (often an abstract syntax tree) and the end index,
    // which is one position after the last character used.
    type Parser[A] = (String, Int) => Option[(A,Int)]

    def parse[A](p: Parser[A], str: String): Option[A] = {
      //println(("parse str | ", str))
      /*val newOpt: Option[(A,Int)] = p(str,0).filter(x => x._1 != None || x._2 > str.length-1)
      newOpt.map(tuple => tuple._1)*/
      p(str,0).filter(tup => tup._2 == str.length).map(tup => tup._1)
    }

    def epsilon[A](a: A): Parser[A] = { (str,i) =>
      //println(("epsilon a, str, i | ",a,str,i))
      /*if (str.length != 0) {
        //println(("epsilon none a,str,i | ", a, str,i))
        None
      } else {
        //println(("epsilon some a,str,i | ", a,str,i))
        Some((a,i))
      }*/

      Some((a,i))
    }

    def any: Parser[Char] = { (str,i) =>
      //println(("any str,i | ", str,i))
      /*if (str.length == 0 || i != str.length-1) None
      else Some((str(i),i+1))*/
      if (i< str.length) Some((str(i),i+1))
      else None
    }

    def char(ch: Char): Parser[Char] = { (str,i) =>
      //println(("char ch,str,i | ", ch,str,i))
      /*if (str == "") None
      else {
          if ( i == str.length-1){
            if (str(i) != ch ) None
            else Some((str(i),i))
        } else {
            if (str(i) != ch ) None
            else Some((str(i),i+1))
        }
      }*/
      /*if (i< str.length && str(i ==ch)) Some((str(i),i+1))
      else None*/
      any(str,i).filter(_._1 == ch)

    }

    def alt[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = { (str,i) =>
      /*if (p1(str,i) == None) p2(str,i)
      else p1(str,i)*/
      p1(str,i) orElse p2(str,i)
    }

    def concat[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] = { (str,i) =>
      val p1Opt:Option[(A,Int)] = p1(str,i)
      if (p1Opt == None) {
        //println(("p1 str and p1 int", str, i))
        None
      }
      else {
        val p2Opt:Option[(B,Int)] = p2(str,p1(str,i).get._2)
        if (p2Opt == None) None
        else {
          Some(((p1Opt.get._1,p2Opt.get._1),p2Opt.get._2))
        }
      }

      p1(str,i) match {
        case None => None
        case Some((a,i2)) => p2(str,i2).map(tup => ((a,tup._1),tup._2))
      }
    }

    def map[A,B](p: Parser[A], f: A => B): Parser[B] = { (str,i) =>
      /*val pOpt:Option[(A,Int)] = p(str,i)
      if (pOpt == None) None
      else {
        Some((f(pOpt.get._1),i ))
      }*/
      p(str,i).map(tup => (f(tup._1),tup._2))
    }

    // tests

    // most of the tests below are deactivated until you're ready for them
    // activate a test by changing the word 'ignore' to say 'test'

    test("epsilon") {
      val p = epsilon(17)
      assertResult(Some(17))(parse(p,""))
      assertResult(None)(parse(p,"a"))
      assertResult(None)(parse(p,"abc"))
    }
    test("any") {
      assertResult(Some('a'))(parse(any, "a"))
      assertResult(Some('b'))(parse(any, "b"))
      assertResult(None)(parse(any, ""))
      assertResult(None)(parse(any, "ab"))
    }
    test("char") {
      assertResult(Some('a'))(parse(char('a'), "a"))
      assertResult(Some('b'))(parse(char('b'), "b"))
      assertResult(None)(parse(char('a'), "b"))
      assertResult(None)(parse(char('a'), ""))
      assertResult(None)(parse(char('a'), "ab"))
    }
    test("alt") {
      val p = alt(char('a'), char('b'))
      assertResult(Some('a'))(parse(p,"a"))
      assertResult(Some('b'))(parse(p,"b"))
      assertResult(None)(parse(p,"c"))
      assertResult(None)(parse(p,""))
      assertResult(None)(parse(p,"ab"))

      val p2 = alt(char('a'),epsilon('E'))
      assertResult(Some('a'))(parse(p2,"a"))
      assertResult(Some('E'))(parse(p2,""))
      assertResult(None)(parse(p2,"b"))
      assertResult(None)(parse(p2,"E"))
      assertResult(None)(parse(p2,"ab"))
    }
    test("concat") {
      val p = concat(char('a'), char('b'))
      assertResult(Some(('a','b')))(parse(p,"ab"))
      assertResult(None)(parse(p,""))
      assertResult(None)(parse(p,"a"))
      assertResult(None)(parse(p,"b"))
      assertResult(None)(parse(p,"ba"))
      assertResult(None)(parse(p,"aba"))
      assertResult(None)(parse(p,"aab"))
      assertResult(None)(parse(p,"abb"))
      assertResult(None)(parse(p,"abc"))

      val p2 = concat(epsilon(5),p)
      assertResult(Some((5,('a','b'))))(parse(p2,"ab"))
      assertResult(None)(parse(p2,""))

      assertResult(Some((('a','b'),('a','b'))))(parse(concat(p,p),"abab"))
    }
    test("map") {
      val pa = map[Char,Char](char('a'), _.toUpper)
      assertResult(Some('A'))(parse(pa, "a"))
      assertResult(None)(parse(pa, "b"))
      assertResult(None)(parse(pa, ""))
      assertResult(None)(parse(pa, "ab"))
    }
    test("Polish") {
      type CII = (Char, (Int, Int))
      def pexpr: Parser[Int] =
        alt(pdigit,
        alt(map[CII,Int](concat(char('+'), concat(pexpr, pexpr)), r => r._2._1 + r._2._2),
            map[CII,Int](concat(char('*'), concat(pexpr, pexpr)), r => r._2._1 * r._2._2)))

      def pdigit: Parser[Int] = {
        val ds: Seq[Parser[Int]] =
          ('0' to '9').map(d => map[Char,Int](char(d), _ => d - '0'))
        ds.tail.foldLeft[Parser[Int]](ds.head)((p1, p2) => alt(p1,p2))
      }

      def polish(str: String): Option[Int] = parse(pexpr, str)

      assertResult(Some(5))(polish("5"))
      assertResult(Some(7))(polish("7"))
      assertResult(Some(8))(polish("+53"))
      assertResult(Some(16))(polish("+79"))
      assertResult(Some(15))(polish("*53"))
      assertResult(Some(63))(polish("*79"))
      assertResult(Some(14))(polish("+2*34"))
      assertResult(Some(10))(polish("+*234"))
      assertResult(Some(60))(polish("*+23+57"))
      assertResult(None)(polish(""))
      assertResult(None)(polish("12"))
      assertResult(None)(polish("+"))
      assertResult(None)(polish("+1"))
      assertResult(None)(polish("+123"))
      assertResult(None)(polish("*"))
      assertResult(None)(polish("*3"))
      assertResult(None)(polish("*345"))
      assertResult(None)(polish("*+**1234"))
      assertResult(None)(polish("*+*23*1234"))
    }

    def main(args: Array[String]): Unit = {
      println(userName);
      execute()
    }
  }
