package properties

import java.nio.file.Path
import java.util.*

class ProjectProperties {
    companion object {
        val mainProperties: Properties
            get() = getProperties("/main.properties")
        val sheetsProperties: Properties
            get() = getProperties("/sheets.properties")

        private fun getProperties(name: String): Properties {
            val properties = Properties()
            try {
                properties.load(ProjectProperties::class.java.getResourceAsStream(name))
            } catch (e: Exception) {
                throw NoSuchFileException(Path.of("src/main/resources/$name").toFile())
            }
            return properties
        }
    }
}