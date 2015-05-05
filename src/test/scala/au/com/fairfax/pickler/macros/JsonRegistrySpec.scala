package au.com.fairfax.pickler.macros

import au.com.fairfax.pickler.au.com.fairfax.mock.{Algorithm, Regression}
import au.com.fairfax.pickler.macros.JsonRegistry._
import au.com.fairfax.pickler.playjson._

import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.{Json => PlayJson}

import scala.language.{implicitConversions, postfixOps}

sealed trait SealedTrait1

case object TraitCaseObject1 extends SealedTrait1

sealed trait SealedTrait2

case object TraitCaseObject21 extends SealedTrait2

case object TraitCaseObject22 extends SealedTrait2

sealed trait SealedTrait3

case object TraitCaseObject31 extends SealedTrait3

case class TraitCaseClass32(a: String) extends SealedTrait3

sealed trait SealedTrait4

case object TraitCaseObject41 extends SealedTrait4

case class TraitCaseClass42(a: String, trait2: SealedTrait2, trait3: SealedTrait3) extends SealedTrait4

sealed trait SealedTrait5

sealed trait SealedTrait51 extends SealedTrait5

case object TraitCaseObject51 extends SealedTrait5

case object TraitCaseObject52 extends SealedTrait51

case class TraitCaseClass51(a: String, trait1: SealedTrait1, caseClass52: CaseClass52) extends SealedTrait5

case class CaseClass52(list1: List[SealedTrait3])

object TraitWrapper {

  sealed trait SealedTrait6

  case object TraitCaseObject61 extends SealedTrait6

  case class TraitCaseClass62(a: String) extends SealedTrait6

}

case class ListString(list: List[String])

case class ListInt(list: List[Int])

case class SeqInt(seq: Seq[Int])

case class VectorInt(v: Vector[Int])

case class IntMapInt(map: Int Map Int)

case class ListSealedTrait2(list: List[SealedTrait2])

case class IntMapSealedTrait2(map: Int Map SealedTrait2)

case class IntMapSealedTrait3List(map: Int Map List[SealedTrait3])

case class IntMapListSealedTrait2(map: Int Map ListSealedTrait2)

case class OptionListInt(list: Option[List[Int]])

case class OptionSealedTrait3(sealedTrait: Option[SealedTrait3])

case class OptionTraitWrapperSealedTrait6(sealedTrait: Option[TraitWrapper.SealedTrait6])

case class EitherContainer(either: Either[EitherLeft, SealedTrait1])

case class EitherLeft(value: SealedTrait2)

case class SampleClass2(i: Short)

case class SampleClass21(c: SampleClass2)

case class AlgoWrapper(learning: Algorithm.Learning)

case class RegressionWrapper(classification: Regression.Classification)

class JsonRegistrySpec extends FlatSpec with Matchers {

  registerTypes()

  it should "have AlgoWrapper containing Algorithm.Learning enum objects formatted/parsed successfully" in {
    val algo = AlgoWrapper(Algorithm.Supervised)
    val formatted = format(algo)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.AlgoWrapper","args":{"learning":"SUPERVISED"}}"""))
    parse(formatted) should be(algo)
  }
  
  it should "have RegressionWrapper containing Regression.Classification enum objects formatted/parsed successfully" in {
    val reg = RegressionWrapper(Regression.Logistic())
    val formatted = format(reg)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.RegressionWrapper","args":{"classification":"LOGISTIC"}}"""))
    parse(formatted) should be(reg)
  }

  it should "have case object empty case class (registered) formatted/parse successfully" in {
    case object CaseObject
    case class CaseClass()
    register[CaseObject.type]
    register[CaseClass]

    val jsonStr = s"""{"t":"CaseObject.type","args":""}"""
    format(CaseObject) should be(PlayJson.parse(jsonStr))
    parse(format(CaseObject)) should be(CaseObject)

    val caseClass = CaseClass()
    format(caseClass) should be(PlayJson.parse(jsonStr replace("CaseObject.type", "CaseClass")))
    parse(format(caseClass)) should be(caseClass)
  }

  it should "have hybrid case object empty case class formatted/parsed successfully" in {
    sealed trait HybridTrait1
    case class HybridCaseObject11() extends HybridTrait1
    case object HybridCaseObject12 extends HybridTrait1
    register[HybridTrait1]

    var hybridTrait1: HybridTrait1 = new HybridCaseObject11
    var formatted = format(hybridTrait1)
    var jsonStr = """{"t":"HybridTrait1","args":{"t":"HybridCaseObject11","v":""}}"""
    formatted should be(PlayJson.parse(jsonStr))
    parse(formatted) should be(hybridTrait1)

    formatted = PlayJson.parse(jsonStr.replace("HybridCaseObject11", "HybridCaseObject12"))
    hybridTrait1 = HybridCaseObject12
    parse(formatted) should be(hybridTrait1)
    parse(format(hybridTrait1)) should be(hybridTrait1)

    // even though only registers HybridTrait1, it should still have the parser/formatter for HybridCaseObject12, HybridCaseObject11
    parse(format(HybridCaseObject12)) should be(HybridCaseObject12)
    parse(format(new HybridCaseObject11)) should be(new HybridCaseObject11)

    sealed trait HybridTrait2
    case class HybridCaseObject21() extends HybridTrait2
    case object HybridCaseObject22 extends HybridTrait2
    case class HybridClass(s: String) extends HybridTrait2
    register[HybridTrait2]

    var hybridTrait2: HybridTrait2 = new HybridCaseObject21
    formatted = format(hybridTrait2)
    jsonStr = """{"t":"HybridTrait2","args":{"t":"HybridCaseObject21","v":""}}"""
    formatted should be(PlayJson.parse(jsonStr))
    parse(formatted) should be(hybridTrait2)

    formatted = PlayJson.parse(jsonStr.replace("HybridCaseObject21", "HybridCaseObject22"))
    hybridTrait2 = HybridCaseObject22
    parse(formatted) should be(hybridTrait2)
    parse(format(hybridTrait2)) should be(hybridTrait2)

    // even though only registers HybridTrait2, it should still have the parser/formatter for HybridCaseObject21, HybridCaseObject22
    parse(format((new HybridCaseObject21))) should be(new HybridCaseObject21)
    parse(format(HybridCaseObject22)) should be(HybridCaseObject22)
  }

  it should "have EitherContainer with left formatted/parsed successfully" in {
    val either: EitherContainer = EitherContainer(Left(EitherLeft(TraitCaseObject21)))
    val formatted = format(either)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.EitherContainer","args":{"either":{"t":"au.com.fairfax.pickler.macros.EitherLeft","v":{"value":{"t":"TraitCaseObject21","v":""}}}}}"""))
    parse(formatted) should be(either)
  }

  it should "have EitherContainer with right formatted/parsed successfully" in {
    val either: EitherContainer = EitherContainer(Right(TraitCaseObject1))
    val formatted = format(either)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.EitherContainer","args":{"either":{"t":"au.com.fairfax.pickler.macros.SealedTrait1","v":{"t":"TraitCaseObject1","v":""}}}}"""))
    parse(formatted) should be(either)
  }

  it should "have ListInt formatted/parsed successfully " in {
    var list = ListInt(List(1, 2, 3))
    val formatted = format(list)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.ListInt","args":{"list":[1.0,2.0,3.0]}}"""))
    parse(formatted) should be(list)

    list = ListInt(null)
    a[IllegalArgumentException] should be thrownBy {
      format(list)
    }

    list = null
    a[IllegalArgumentException] should be thrownBy {
      format(list)
    }
  }

  it should "have ListString formatted/parsed successfully " in {
    val list = ListString(List("a", "b", "c"))
    val formatted = format(list)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.ListString","args":{"list":["a","b","c"]}}"""))
    parse(formatted) should be(list)
  }

  it should "have SeqInt formatted/parsed successfully " in {
    val seq = SeqInt(Seq(1, 2, 3))
    val formatted = format(seq)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.SeqInt","args":{"seq":[1.0,2.0,3.0]}}"""))
    parse(formatted) should be(seq)
  }

  it should "have VectorInt formatted/parsed successfully " in {
    val vector = VectorInt(Vector(1, 2, 3))
    val formatted = format(vector)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.VectorInt","args":{"v":[1.0,2.0,3.0]}}"""))
    parse(formatted) should be(vector)
  }

  it should "have Short formatted/parsed successfully " in {
    val i: Short = 99
    val formatted = format(i)
    formatted should be(PlayJson.parse( """{"t":"Short","args":99.0}"""))
    parse(formatted) should be(i)
  }

  it should "have Long formatted/parsed successfully " in {
    val i: Long = 99
    val formatted = format(i)
    formatted should be(PlayJson.parse( """{"t":"Long","args":99.0}"""))
    parse(formatted) should be(i)
  }

  it should "have Double formatted/parsed successfully " in {
    val d = 99.1
    val formatted = format(d)
    formatted should be(PlayJson.parse( """{"t":"Double","args":99.1}"""))
    parse(formatted) should be(d)
  }

  it should "have Float formatted/parsed successfully " in {
    val d = 99.1f
    val formatted = format(d)
    formatted should be(PlayJson.parse( """{"t":"Float","args":99.0999984741211}"""))
    parse(formatted) should be(d)
  }

  it should "have Int formatted/parsed successfully " in {
    val d: Int = 99
    val formatted = format(d)
    formatted should be(PlayJson.parse( """{"t":"Int","args":99.0}"""))
    parse(formatted) should be(d)
  }

  it should "have Boolean formatted/parsed successfully " in {
    val b: Boolean = true
    val formatted = format(b)
    formatted should be(PlayJson.parse( """{"t":"Boolean","args":true}"""))
    parse(formatted) shouldBe b
  }

  it should "have String formatted/parsed successfully " in {
    var s: String = "test"
    val formatted = format(s)
    formatted should be(PlayJson.parse( """{"t":"String","args":"test"}"""))
    parse(formatted) should be(s)

    s = null
    a[IllegalArgumentException] should be thrownBy {
      format(s)
    }
  }

  it should "have SampleClass21 formatted/parsed successfully " in {
    var s = SampleClass21(SampleClass2(1))
    val formatted = format(s)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.SampleClass21","args":{"c":{"i":1.0}}}"""))
    parse(formatted) should be(s)

    s = null
    a[IllegalArgumentException] should be thrownBy {
      format(s)
    }

    s = SampleClass21(null)
    a[IllegalArgumentException] should be thrownBy {
      format(s)
    }
  }


  it should "have IntMapInt formatted/parsed successfully " in {
    var map = IntMapInt(Map(101 -> 202))
    val formatted = format(map)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.IntMapInt","args":{"map":[[101.0,202.0]]}}"""))
    parse(formatted) should be(map)

    map = IntMapInt(null)
    a[IllegalArgumentException] should be thrownBy {
      format(map)
    }

    map = null
    a[IllegalArgumentException] should be thrownBy {
      format(map)
    }
  }

  it should "have SealedTrait1 family formatted/parsed successfully " in {
    val sealedTrait: SealedTrait1 = TraitCaseObject1
    val formatted = format(sealedTrait)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.SealedTrait1","args":{"t":"TraitCaseObject1","v":""}}"""))
    parse(formatted) should be(sealedTrait)
  }

  it should "have ListSealedTrait2 family formatted/parsed successfully " in {
    val list = ListSealedTrait2(List(TraitCaseObject21, TraitCaseObject22))
    val formatted = format(list)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.ListSealedTrait2","args":{"list":[{"t":"TraitCaseObject21","v":""},{"t":"TraitCaseObject22","v":""}]}}"""))
    parse(formatted) should be(list)
  }

  it should "have IntMapSealedTrait2 family formatted/parsed successfully " in {
    val map = IntMapSealedTrait2(Map(1 -> TraitCaseObject21, 2 -> TraitCaseObject22))
    val formatted = format(map)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.IntMapSealedTrait2","args":{"map":[[1.0,{"t":"TraitCaseObject21","v":""}],[2.0,{"t":"TraitCaseObject22","v":""}]]}}"""))
    parse(formatted) should be(map)
  }

  it should "have SealedTrait3 family formatted/parsed successfully " in {
    val sealedTrait: SealedTrait3 = TraitCaseClass32("hey")
    val formatted = format(sealedTrait)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.SealedTrait3","args":{"t":"TraitCaseClass32","v":{"a":"hey"}}}"""))
    parse(formatted) should be(sealedTrait)
  }

  it should "have IntMapSealedTrait3List family formatted/parsed successfully " in {
    val map = IntMapSealedTrait3List(Map(
      1 -> List(TraitCaseClass32("HI"), TraitCaseObject31),
      2 -> List(TraitCaseClass32("what"), new TraitCaseClass32("yes")),
      3 -> List(TraitCaseObject31, TraitCaseObject31)))
    val formatted = format(map)
    formatted should be(PlayJson.parse(
      """
        |{"t":"au.com.fairfax.pickler.macros.IntMapSealedTrait3List","args":{"map":[[1.0,[{"t":"TraitCaseClass32","v":{"a":"HI"}},{"t":"TraitCaseObject31","v":""}]],[2.0,[{"t":"TraitCaseClass32","v":{"a":"what"}},{"t":"TraitCaseClass32","v":{"a":"yes"}}]],[3.0,[{"t":"TraitCaseObject31","v":""},{"t":"TraitCaseObject31","v":""}]]]}}
      """.stripMargin))
    parse(formatted) should be(map)
  }

  it should "have IntMapListSealedTrait2 family formatted/parsed successfully " in {
    val map = IntMapListSealedTrait2(Map(
      1 -> ListSealedTrait2(List(TraitCaseObject21)),
      2 -> ListSealedTrait2(List(TraitCaseObject22)),
      3 -> ListSealedTrait2(List(TraitCaseObject21, TraitCaseObject22))))
    val formatted = format(map)
    formatted should be(PlayJson.parse(
      """
        |{"t":"au.com.fairfax.pickler.macros.IntMapListSealedTrait2","args":{"map":[[1.0,{"list":[{"t":"TraitCaseObject21","v":""}]}],[2.0,{"list":[{"t":"TraitCaseObject22","v":""}]}],[3.0,{"list":[{"t":"TraitCaseObject21","v":""},{"t":"TraitCaseObject22","v":""}]}]]}}
      """.stripMargin))
    parse(formatted) should be(map)
  }

  it should "have SealedTrait4 family formatted/parsed successfully " in {
    var sealedTrait: SealedTrait4 = TraitCaseObject41
    var formatted = format(sealedTrait)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.SealedTrait4","args":{"t":"TraitCaseObject41","v":""}}"""))
    parse(formatted) should be(sealedTrait)

    sealedTrait = TraitCaseClass42("Test4", TraitCaseObject22, TraitCaseClass32("Test3"))
    formatted = format(sealedTrait)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.SealedTrait4","args":{"t":"TraitCaseClass42","v":{"a":"Test4","trait2":{"t":"TraitCaseObject22","v":""},"trait3":{"t":"TraitCaseClass32","v":{"a":"Test3"}}}}}"""))
    parse(formatted) should be(sealedTrait)
  }

  it should "have OptionListInt formatted/parsed successfully" in {
    val option = OptionListInt(Some(List(1, 2, 3)))
    val formatted = format(option)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.OptionListInt","args":{"list":[1.0,2.0,3.0]}}"""))
    parse(formatted) should be(option)
  }

  it should "have OptionSealedTrait3 formatted/parsed successfully" in {
    val option = OptionSealedTrait3(Some(TraitCaseClass32("testing OptionSealedTrait3")))
    val formatted = format(option)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.OptionSealedTrait3","args":{"sealedTrait":{"t":"TraitCaseClass32","v":{"a":"testing OptionSealedTrait3"}}}}"""))
    parse(formatted) should be(option)
  }

  it should "have OptionTraitWrapperSealedTrait6 formatted/parsed successfully" in {
    var option = OptionTraitWrapperSealedTrait6(Some(TraitWrapper.TraitCaseClass62("testing OptionTraitWrapperSealedTrait6")))
    var formatted = format(option)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.OptionTraitWrapperSealedTrait6","args":{"sealedTrait":{"t":"TraitWrapper.TraitCaseClass62","v":{"a":"testing OptionTraitWrapperSealedTrait6"}}}}"""))
    parse(formatted) should be(option)

    option = OptionTraitWrapperSealedTrait6(None)
    formatted = format(option)
    formatted should be(PlayJson.parse( """{"t":"au.com.fairfax.pickler.macros.OptionTraitWrapperSealedTrait6","args":{"sealedTrait":null}}"""))
    parse(formatted) should be(option)
  }

  it should "have List[Int] formatted/parsed successfully (for testing generics coped by TypeKeyProvider on the top level)" in {
    val ints = List(1, 2, 3)
    parse(format(ints)) should be(ints)

    // not registered List[Float] yet, should be failed
    val floats = List(1f, 2f)
    a[Error] should be thrownBy {
      format(floats)
    }
  }

  it should "have List[TraversableCollection] formatted/parsed successfully to test TraversableRegistrar on pattern match collection type" in {
    case class TraversableCollection(s: String)
    register[List[TraversableCollection]]

    val list = List(TraversableCollection("a"), TraversableCollection("b"))
    parse(format(list)) should be(list)

    parse(format(list.head)) should be(list.head)
  }

  it should "have Map[TraversableKey, TraversableValue] formatted/parsed successfully to test TraversableRegistrar on pattern match map type" in {
    case class TraversableKey(i: Int)
    case class TraversableValue(s: String)
    register[TraversableKey Map TraversableValue]

    val k = TraversableKey(1)
    val v = TraversableValue("abc")
    val map = Map(k -> v)
    parse(format(map)) == map
    parse(format(k)) == k
    parse(format(v)) == v
  }

  it should "have Option[TraversableOption] formatted/parsed successfully to test TraversableRegistrar on pattern match option type" in {
    case class TraversableOption(s: String)
    register[Option[TraversableOption]]

    var opt: Option[TraversableOption] = Some(TraversableOption("abc"))
    parse(format(opt)) == opt
    parse(format(opt.get)) == opt.get

    opt = None
    parse(format(opt)) == opt
  }

  it should "have Either[TraversableLeft, TraversableRight] formatted/parsed successfully to test TraversableRegistrar on pattern match either type" in {
    case class TraversableLeft(l: String)
    case class TraversableRight(r: String)
    register[Either[TraversableLeft, TraversableRight]]

    val left = TraversableLeft("democrat")
    var either: Either[TraversableLeft, TraversableRight] = Left(left)

    parse(format(either)) == either
    parse(format(left)) == left

    val right = TraversableRight("republican")
    either = Right(right)

    parse(format(either)) == either
    parse(format(right)) == right
  }


  it should "have TraversableTrait formatted/parsed successfully to test TraversableRegistrar on pattern match trait type" in {
    sealed trait TraversableTrait
    case class TraversableCaseClass1(s1: String) extends TraversableTrait
    case class TraversableCaseClass2(s2: String) extends TraversableTrait
    register[TraversableTrait]

    val t1 = TraversableCaseClass1("s1")
    parse(format(t1)) == t1

    val t2 = TraversableCaseClass2("s2")
    parse(format(t2)) == t2
  }

  it should "have TraversableStructure formatted/parsed successfully to test TraversableRegistrar on pattern match structured type" in {
    case class TraversableChild1(s1: String)
    case class TraversableChild2(s2: String)
    case class TraversableStructure(c1: TraversableChild1, c2: TraversableChild2)
    register[TraversableStructure]

    val c1 = TraversableChild1("child1")
    val c2 = TraversableChild2("child2")
    val struct = TraversableStructure(c1, c2)

    parse(format(struct)) == struct
    parse(format(c1)) == c1
    parse(format(c2)) == c2
  }

  it should "have structure whose # fields not the same as old json but still formatted/parsed successfully" in {
    // json data has more field than data object
    case class Sample1(m: String)
    register[Sample1]
    parse(PlayJson.parse( """{"t":"Sample1","args":{"m":"hay","n":"haha"}}""")) should be {
      Sample1("hay")
    }

    // json data has more field than data object, and the data object field is Option
    case class Sample2(m: Option[String])
    register[Sample2]
    parse(PlayJson.parse( """{"t":"Sample2","args":{"m":"hay","n":"haha"}}""")) should be {
      Sample2(Some("hay"))
    }

    // json data has fewer fields than data object
    case class Sample3(m: Option[String], n: Option[String])
    register[Sample3]
    parse(PlayJson.parse( """{"t":"Sample3","args":{"n":"hay"}}""")) should be {
      Sample3(None, Some("hay"))
    }

    // object has fewer fields than its json counterpart is enclosed by List
    register[List[Sample1]]
    parse(PlayJson.parse(
      """
        |{"t":"List[Sample1]","args":[{"m":"hay","n":"haha"},{"m":"hay1","n":"haha1"}]}
        | """.stripMargin)) should be {
      List(Sample1("hay"), Sample1("hay1"))
    }

    // object has more fields than its json counterpart is enclosed by List
    case class Sample4(m: String, n: Option[String])
    register[List[Sample4]]
    parse(PlayJson.parse(
      """
        |{"t":"List[Sample4]","args":[{"m":"hay"},{"m":"hay1"}]}
        | """.stripMargin)) should be {
      List(Sample4("hay", None), Sample4("hay1", None))
    }

    // object has same # of fields but its json counterpart has some null value
    parse(PlayJson.parse(
      """
        |{"t":"List[Sample4]","args":[{"m":"hay","n":"haha1"},{"m":"hay1","n":null}]}
        | """.stripMargin)) should be {
      List(Sample4("hay", Some("haha1")), Sample4("hay1", None))
    }
  }

  def registerTypes() = {
    register[AlgoWrapper]
    register[RegressionWrapper]
    register[EitherContainer]
    register[OptionListInt]
    register[OptionSealedTrait3]
    register[OptionTraitWrapperSealedTrait6]
    register[ListString]
    register[ListInt]
    register[SeqInt]
    register[VectorInt]
    register[Short]
    register[Long]
    register[Double]
    register[Float]
    register[Int]
    register[Boolean]
    register[String]
    register[SampleClass21]
    register[IntMapInt]
    register[SealedTrait4]
    register[SealedTrait1]
    register[ListSealedTrait2]
    register[IntMapSealedTrait2]
    register[SealedTrait3]
    register[IntMapSealedTrait3List]
    register[IntMapListSealedTrait2]
    register[SealedTrait5]
    register[TraitWrapper.SealedTrait6]
    register[List[Int]]
  }
}