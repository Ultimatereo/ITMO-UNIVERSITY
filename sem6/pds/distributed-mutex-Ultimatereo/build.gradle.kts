
group = "ru.ifmo.pds"
version = "1.0-SNAPSHOT"

plugins {
    kotlin("jvm") version "1.9.22"
}

repositories {
    mavenCentral()
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(21)
    }
}

dependencies {
    implementation("ch.qos.logback:logback-classic:1.2.10")
    testImplementation(kotlin("test-junit"))
}

sourceSets["main"].java.setSrcDirs(listOf("src"))
sourceSets["test"].java.setSrcDirs(listOf("test"))

val processId = project.properties["processId"] as? String ?: "1"
val implName = project.properties["implName"] as? String ?: "ProcessImpl"

tasks.test {
    testLogging.showStandardStreams = true
    systemProperty("implName", implName)
    project.properties["skipCountCheck"]?.let { systemProperty("skipCountCheck", it) }
}

tasks.register<JavaExec>("node") {
    javaLauncher = javaToolchains.launcherFor(java.toolchain)
    classpath = sourceSets["main"].runtimeClasspath
    mainClass.set("mutex.system.NodeKt")
    args = listOf(processId, implName)
    standardInput = System.`in`
}

tasks.register<JavaExec>("system") {
    javaLauncher = javaToolchains.launcherFor(java.toolchain)
    classpath = sourceSets["main"].runtimeClasspath
    mainClass.set("mutex.system.SystemKt")
    args = listOf(implName)
    standardInput = System.`in`
}
