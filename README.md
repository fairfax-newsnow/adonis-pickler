#Adonis pickler
Adonis pickler is a library that serializes Scala object to Json format and vice versa (aka de-serialization).  It is designed for the Fairfax's project and cope with the following data types:
* primitive types, i.e. float, short, double, boolean, int, string
* collection
* map
* option
* either
* case class
* sealed trait
* case object
* empty case class
* generics
* self-defined enum object which is an attribute inside a case class
* Limited support for an attribute of Any type inside a case class

This pickler can also be compiled to a js library using ScalaJs such that it can be used by js applications executed in a browsers.  Implicit materializer macro is used in writing this pickler.  You are assumed to have basic understanding of Scala macros and the implicit materializer macro.  Please refer to http://docs.scala-lang.org/overviews/macros/implicits.html for more information.

##Json format
Adonis pickler is designed to provide just enough information in the Json data to minimize the overhead while enable it to be de-serialized to Scala object.
Let's have a quick look on the various Json formats of some sample Scala data by running the following command under the folder `adonis-pickler`
```scala
sbt
>test:console
scala> import au.com.fairfax.pickler.playjson._
import au.com.fairfax.pickler.playjson._

scala> import au.com.fairfax.pickler.macros.JsonRegistry._
import au.com.fairfax.pickler.macros.JsonRegistry._
```
###Primitive data type
```scala
scala> register[Int]

scala> format(123)
res1: play.api.libs.json.JsValue = {"t":"Int","args":123.0}

scala> parse(format(123)) == 123
res2: Boolean = true
```
`register[Int]` triggers code generation of serializer (aka formatter) and de-serializer (aka parser) to a data type of `Int`, stores the generated formatter/parser in `JsonRegistry`
`format(123)` detects the compiled type of 123 is `Int`, looks up the generated formatter of `Int` from `JsonRegistry`
`parse(JsValue)` does the corresponding task using parser
###Collection
```Scala
scala> register[List[Int]]

scala> format(List(1, 2, 3))
res4: play.api.libs.json.JsValue = {"t":"List[Int]","args":[1.0,2.0,3.0]}
```
###Map
```Scala
scala> register[Int Map String]

scala> format(Map(1 -> "One"))
res6: play.api.libs.json.JsValue = {"t":"Map[Int,String]","args":[[1.0,"One"]]}
```
###Option
```Scala
scala> register[Option[String]]

scala> format(Some("abc"):Option[String])
res9: play.api.libs.json.JsValue = {"t":"Option[String]","args":"abc"}

scala> format(None:Option[String])
res10: play.api.libs.json.JsValue = {"t":"Option[String]","args":null}
```
###Either
```Scala
scala> register[Either[Int, String]]

scala> format(Left(123): Either[Int, String])
res14: play.api.libs.json.JsValue = {"t":"Either[Int,String]","args":{"t":"Int","v":123.0}}

scala> format(Right("abc"): Either[Int, String])
res15: play.api.libs.json.JsValue = {"t":"Either[Int,String]","args":{"t":"String","v":"abc"}}
```
###Case class
Ignor `<console>:15: warning: match may not be exhaustive.`  It is due to the quaisquote in 
```Scala
scala> case class Sample4(m: String, n: Option[String])
defined class Sample4

scala> register[Sample4]

scala> format(Sample4("hey", Some("um?")))
res17: play.api.libs.json.JsValue = {"t":"Sample4","args":{"m":"hey","n":"um?"}}
```
###Sealed trait
```Scala
scala> sealed trait SealedTrait
defined trait SealedTrait

scala> case object CaseObject1 extends SealedTrait
defined object CaseObject1

scala> case class FakeCaseObject() extends SealedTrait
defined class FakeCaseObject

scala> case class CaseClass1(m: String, n: Option[String]) extends SealedTrait
defined class CaseClass1

scala> register[SealedTrait]

scala> format(CaseObject1: SealedTrait)
res19: play.api.libs.json.JsValue = {"t":"SealedTrait","args":{"t":"CaseObject1","v":""}}

scala> format(FakeCaseObject(): SealedTrait)
res20: play.api.libs.json.JsValue = {"t":"SealedTrait","args":{"t":"FakeCaseObject","v":""}}

scala> format(CaseClass1("abc", Some("no!!!")): SealedTrait)
res21: play.api.libs.json.JsValue = {"t":"SealedTrait","args":{"t":"CaseClass1","v":{"m":"abc","n":"no!!!"}}}
scala> 
```
###Case object
Sometimes a case object will be formatted directly rather than via a sealed trait.
```Scala
scala> case object CaseObject
defined object CaseObject

scala> register[CaseObject.type]

scala> format(CaseObject)
res2: play.api.libs.json.JsValue = {"t":"CaseObject.type","args":""}
```
###Empty case class
```Scala
scala> case class FakeCaseObject()
defined class FakeCaseObject

scala> register[FakeCaseObject]

scala> format(FakeCaseObject())
res4: play.api.libs.json.JsValue = {"t":"FakeCaseObject","args":""}
```
###Generics
```Scala
scala> case class ForGeneric[T](attr: T){
     |   def toMsg: String = attr.toString
     | }
defined class ForGeneric

scala> register[ForGeneric[Int]]

scala> format(ForGeneric(123))
res6: play.api.libs.json.JsValue = {"t":"ForGeneric[Int]","args":{"attr":123.0}}

scala> format(ForGeneric("abc"))
java.lang.Error: No formatter exists for ForGeneric[String]
  at au.com.fairfax.pickler.macros.BaseJsonRegistry$$anonfun$format$2.apply(JsonRegistry.scala:53)
  at au.com.fairfax.pickler.macros.BaseJsonRegistry$$anonfun$format$2.apply(JsonRegistry.scala:53)
  at scala.Option.fold(Option.scala:158)
  at au.com.fairfax.pickler.macros.BaseJsonRegistry.format(JsonRegistry.scala:54)
  ... 43 elided
```
It fails in the second `format` because it has never `register[ForGeneric[String]]`
###Self-defined enum object
For the time being, supporting enum value needs to
* Use self-defined `au.com.fairfax.pickler.types.Enum`.  E.g. `Algorithm.Learning` and `Regression.Classification` https://github.com/fairfax-newsnow/adonis-pickler/blob/master/src/test/scala/au/com/fairfax/pickler/au/com/fairfax/mock/Taxonomy.scala.
* Be an attribute of a case class

```Scala
scala> import au.com.fairfax.pickler.au.com.fairfax.mock._
import au.com.fairfax.pickler.au.com.fairfax.mock._

scala> case class MlWrapper(algo: Algorithm.Learning, reg: Regression.Classification)
defined class MlWrapper

scala> register[MlWrapper]

scala> format(MlWrapper(Algorithm.Supervised, Regression.Logistic()))
res1: play.api.libs.json.JsValue = {"t":"MlWrapper","args":{"algo":"SUPERVISED","reg":"LOGISTIC"}}
```
###Any type inside a case class
If a case class having an attribute of type `Any`, it can be supported provided the runtime type of the attribute is:
* A primitive type
* Does not contain generic, e.g. `List[Int]`.  Because generics information can only be captured in compiled time.

```Scala
scala> case class AnyWrapper(any: Any)
defined class AnyWrapper

scala> register[AnyWrapper]

scala> register[Int]

scala> format(AnyWrapper(123))
res2: play.api.libs.json.JsValue = {"t":"AnyWrapper","args":{"any":{"t":"Int","v":123.0}}}

scala> format(AnyWrapper(123L))
java.lang.Error: No formatter exists for Any or java.lang.Long derived from object of class name class java.lang.Long.replace('$', '.') or Long
  at au.com.fairfax.pickler.macros.BaseJsonRegistry$$anonfun$internalFormat$1$$anonfun$apply$1$$anonfun$apply$2.apply(JsonRegistry.scala:75)
  at au.com.fairfax.pickler.macros.BaseJsonRegistry$$anonfun$internalFormat$1$$anonfun$apply$1$$anonfun$apply$2.apply(JsonRegistry.scala:75)
  at scala.Option.fold(Option.scala:158)
  at au.com.fairfax.pickler.macros.BaseJsonRegistry$$anonfun$internalFormat$1$$anonfun$apply$1.apply(JsonRegistry.scala:76)
  at scala.Option.fold(Option.scala:158)
  at au.com.fairfax.pickler.macros.BaseJsonRegistry$$anonfun$internalFormat$1.apply(JsonRegistry.scala:77)
  at scala.Option.fold(Option.scala:158)
  at au.com.fairfax.pickler.macros.BaseJsonRegistry.internalFormat(JsonRegistry.scala:78)
  at GenTraversableRegistrar$2$GenJsonFormatter$2$.format(<console>:16)
  at au.com.fairfax.pickler.macros.BaseJsonRegistry$$anonfun$format$3.apply(JsonRegistry.scala:54)
  at au.com.fairfax.pickler.macros.BaseJsonRegistry$$anonfun$format$3.apply(JsonRegistry.scala:54)
  at scala.Option.fold(Option.scala:158)
  at au.com.fairfax.pickler.macros.BaseJsonRegistry.format(JsonRegistry.scala:54)
  ... 43 elided
```
It fails in the second `format` because the pickler detects the runtime class of the first attribute is `Long` but it has not `register[Long]`

##How to use the pickler
As showed in the previous section, using the pickler is pretty straight forward - calls the code generation of parser/formatter of a specific type by `register[SpecificScalaDataType]`.
* Register a type that has been registered before will not generate the code again.
* The order of registering types does not matter.
* It is good habit to register all the required data types by calling `register[SpecificScalaDataType]` inside a specific object.

Please refer to https://github.com/fairfax-newsnow/adonis-pickler/blob/master/src/test/scala/au/com/fairfax/pickler/macros/JsonRegistrySpec.scala as reference.
##Application structure
###JReader
A trait that reads the attributes from the Json data.  There are different implementation APIs for reading the Json such as PlayJson, ScalaJs and so on.  Therefore the type of the Json data type is parametrized as generic.  It is used by `JsonParser[SpecificScalaDataType]` implementation.
###ParserMaterializerImpl
A materializer macro responsible for generating the code that implements `JsonParser[SpecificScalaDataType]` for data type `SpecificScalaDataType`
###JBuilder
Similar to `JReader`.  This trait builds up the attribute values for the Json data.  It is used by `JsonFormatter[SpecificScalaDataType]` implementation.
###FormatterMaterializerImpl
A materializer macro responsible for generating the code that implements `JsonFormatter[SpecificScalaDataType]` for data type `SpecificScalaDataType`
###Materializer
A trait that defines the common skeleton for both `ParserMaterializerImpl` and `FormatterMaterializerImpl`
###JsonRegistry
It is the entry class that a pickler client interacts with and maintains a registry for all the generated parsers/formatters.  It provides the following methods for a pickler client.
* `register`
* `format`
* `parse`

###ParserFormatterTracker
A singleton that keeps track of which Scala data type's parser/formatter have been generated during Scala code compilation.
###TraversableRegistrar
It is the class where materializer macro is triggered to generate the code.  Its client is `JsonRegistry`.  When `JsonRegistry.register[SpecificDataType]` is called, it will implicitly load `TraversableRegistrar[SpecificDataType]` implementation whose code is generated as follows.
* Checks from `ParserFormatterTracker` if `SpecificDataType`'s parser/formatter have been generated before.  If no, it will call `ParserMaterializerImpl.parserQuote` and `FormatterMaterializerImpl.formatterQuote`.
* Checks all the data types which are directly or indirectly related to `SpecificDataType`.  For each related data type, checks from `ParserFormatterTracker` if the data type's parser/formatter have been generated before and generate the code if necessary.

```Scala
scala> case class Sample(i: String)
defined class Sample

scala> register[List[Sample]]

scala> register[Int Map Double]

scala> format("abc")
res2: play.api.libs.json.JsValue = {"t":"String","args":"abc"}

scala> format(0.2)
res3: play.api.libs.json.JsValue = {"t":"Double","args":0.2}
```
* `register[List[Sample]]` generates parsers/formatters of `List[Sample]`, `Sample` and `String`.
* `register[Int Map Double]` generates parsers/formatters of `Int Map Double`, `Int` and `Double`.
* `format("abc")` does not need to `register[String]` beforehand because `String` parser/formater has been generated in `register[List[Sample]]`.
* Similar theory applies to `format(0.2)`.

###TypeKeyProvider
In formatting a Scala data object such as `format("abc")`, it needs to check the compile type of `"abc"` which is `String` to look up the corresponding formatter from `JsonRegistry`.  The type can be found by simply using `TypeTag`.  However, the ScalaJs library for the time being does not support `TypeTag`.  `TypeKeyProvider` is a workaround for generating code that provides the type information.