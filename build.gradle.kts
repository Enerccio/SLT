plugins {
    id("java")
    id("org.jetbrains.intellij") version "1.12.0"
}

group = "com.en_circle.slt"
version = "0.2.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.awaitility:awaitility:4.2.0")
    implementation("org.watertemplate:watertemplate-engine:1.2.2")
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
    version.set("2022.1")
    pluginName.set("slt")
    var ide = System.getenv("TARGET_IDE")
    if (ide == null || "" == ide)
        ide = extra["targetIDE"].toString()
    type.set(ide) // Target IDE Platform

    plugins.set(listOf(

    /* Plugin Dependencies */))
}

tasks {
    // Set the JVM compatibility versions
    withType<JavaCompile> {
        sourceCompatibility = "11"
        targetCompatibility = "11"
    }

    patchPluginXml {
        sinceBuild.set("221")
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
