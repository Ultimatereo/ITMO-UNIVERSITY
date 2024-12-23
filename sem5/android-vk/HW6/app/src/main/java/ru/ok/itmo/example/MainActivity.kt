package ru.ok.itmo.example
import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.Button
import androidx.appcompat.app.AppCompatActivity

class MainActivity : AppCompatActivity() {
    private lateinit var startCoroutinesButton: Button
    private lateinit var startFlowButton: Button
    private lateinit var startStateFlowButton: Button

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        startCoroutinesButton = findViewById(R.id.CoroutinesImpl)
        startFlowButton = findViewById(R.id.FlowImpl)
        startStateFlowButton = findViewById(R.id.StateFlowImpl)
    }

    fun startCoroutines(view: View) {
        startCoroutinesButton.isEnabled = false
        startActivity(Intent(this, KotlinCoroutinesImplementation::class.java))
        startCoroutinesButton.isEnabled = true
    }

    fun startFlow(view: View) {
        startFlowButton.isEnabled = false
        startActivity(Intent(this, KotlinFlowImplementation::class.java))
        startFlowButton.isEnabled = true
    }

    fun startStateFlow(view: View) {
        startStateFlowButton.isEnabled = false
        startActivity(Intent(this, StateFlowImplementation::class.java))
        startFlowButton.isEnabled = true
    }
}