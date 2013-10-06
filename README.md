## Validation

This is a simple Scala library for validation. The library is very much similar
to Scalaz Validation but does not overwhelm users with concepts and compiler
errors that can be hard to understand without strong functional background.

## Getting Validation

If you're using SBT, add the following lines to your build file:

	resolvers += "Validation releases" at "http://lfjallstrom.github.io/maven/releases"
	libraryDependencies += "com.github.lfjallstrom" %% "validation" % "1.0.0" % "compile"

## Usage

	import com.github.lfjallstrom.validation._
	
	def parse(i: String) : Validation[String, Int] = {
	  try {
	  	i.toInt.success
	  } catch {
	    case _ : java.lang.NumberFormatException => (s"Not a valid integer ${i}").failure
	  }
	}
	
	def add(s1: String, s2: String) : Validation[String, Int] = {
	  (parse(s1) and parse(s2)) { (i1, i2) =>
	    i1 + i2
	  }
	}
	
	add("1", "2")
	//> res0: com.github.lfjallstrom.validation.Validation[String,Int] = Success(3)
	add("a", "b")
	//> res1: com.github.lfjallstrom.validation.Validation[String,Int] = Failure(List(Not a valid integer a, Not a valid integer b))
	
	add("1", "2").getOrElse(0)
	//> res2: Int = 3
	add("a", "b").getOrElse(0)
	//> res3: Int = 0
	
	add("1", "2").orElse(0.success)
	//> res4: com.github.lfjallstrom.validation.Validation[String,Int] = Success(3)
	add("a", "b").orElse(0.success)
	//> res5: com.github.lfjallstrom.validation.Validation[String,Int] = Success(0)
	
	def getRange(s1: String, s2: String) : Validation[String, Range] = {
	  (parse(s1) and parse(s2)) { (i1, i2) =>
	    if (i1 < i2) Range(i1, i2).success
	    else s"${i1} is not less than ${i2}".failure
	  }.flatMap(identity)
	}
	
	getRange("1", "2")
	//> res6: com.github.lfjallstrom.validation.Validation[String,Range] = Success(Range(1))
	getRange("a", "b")
	//> res7: com.github.lfjallstrom.validation.Validation[String,Range] = Failure(List(Not a valid integer a, Not a valid integer b))
	getRange("2", "1")
	//> res8: com.github.lfjallstrom.validation.Validation[String,Range] = Failure(List(2 is not less than 1))

## License

Licensed under MIT license. See LICENSE.txt file for details. I would be
grateful if you let me know who is using my library though.
