
name := "cats-adjunction"

scalaVersion := "2.12.0"

scalacOptions ++= Seq("-language:higherKinds")

libraryDependencies ++= Seq(
  "org.typelevel"                 %% "cats-core"      % "0.8.1",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
)



