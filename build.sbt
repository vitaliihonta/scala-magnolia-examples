name := "ScalaMagnoliaMeetup"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies ++= {
  Seq(
    "com.typesafe.play" %% "play-json" % "2.7.4",
    "com.propensive" %% "magnolia" % "0.11.0",
    "com.chuusai" %% "shapeless" % "2.3.3"
  )
}

scalacOptions += "-Ypartial-unification"