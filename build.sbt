
name := "cats-adjunction"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-language:higherKinds")

libraryDependencies ++= Seq(
  "com.chuusai"                   %% "shapeless"      % "2.3.0",
  "org.typelevel"                 %% "cats-core"      % "0.4.0",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
)



