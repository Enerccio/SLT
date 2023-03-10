plugins {
    id("java")
    id("org.jetbrains.intellij") version "1.12.0"
}

group = "com.en_circle.slt"
version = "0.4.1-rev1"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.awaitility:awaitility:4.2.0")
    implementation("org.watertemplate:watertemplate-engine:1.2.2")
    implementation("com.google.guava:guava:31.1-jre")
    implementation("org.rauschig:jarchivelib:1.2.0")
    implementation("org.abcl:abcl:1.8.0")

    testImplementation("org.junit.jupiter:junit-jupiter-api:5.9.2")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.9.2")
}

sourceSets {
    main {
        java {
            srcDirs("src/main/gen")
        }
    }
}

// Configure Gradle IntelliJ Plugin
// Read more: https://plugins.jetbrains.com/docs/intellij/tools-gradle-intellij-plugin.html
intellij {
    version.set("2022.2")
    pluginName.set("slt")
    var ide = System.getenv("TARGET_IDE")
    if (ide == null || "" == ide)
        ide = extra["targetIDE"].toString()
    type.set(ide) // Target IDE Platform

    plugins.set(listOf(

    /* Plugin Dependencies */))
}


tasks {
    val sltZip = task("sltZip", Zip::class) {
        from("src/main/lisp")
        archiveFileName.set("slt.zip")
        destinationDirectory.set(File("build/resources/main"))
        include("**/*.lisp")
        include("**/*.cl")
        include("**/*.asdf")
        include("LICENSE.txt")

        doFirst {
            println("Zipping SLT to " + destinationDirectory.get())
        }
        eachFile{
            println("Zipping $this")
        }
        doLast {
            println("Done Zipping SLT")
        }

        outputs.upToDateWhen {
            false
        }
    }
    sltZip.mustRunAfter(processResources)
    jar {
        dependsOn(sltZip)
    }

    // Set the JVM compatibility versions
    withType<JavaCompile> {
        sourceCompatibility = "17"
        targetCompatibility = "17"
    }

    patchPluginXml {
        sinceBuild.set("222")
        untilBuild.set("231.*")
    }

    signPlugin {
        try {
            certificateChain.set(File("./signcerts/chain.crt").readText(Charsets.UTF_8))
            privateKey.set(File("./signcerts/private.pem").readText(Charsets.UTF_8))
            password.set(System.getenv("PRIVATE_KEY_PASSWORD"))
        } catch (_: Exception) {

        }
    }

    publishPlugin {
        token.set(System.getenv("PUBLISH_TOKEN"))
    }
}

