scalaVersion := "2.11.7"

resolvers in ThisBuild ++= Seq(
    Resolver.typesafeRepo("releases")
  , Resolver.typesafeIvyRepo("releases")
  , Resolver.sonatypeRepo("releases")
  , Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.2.5"
  , "org.spire-math" %% "cats" % "0.4.0-SNAPSHOT"
)
