package ru.ok.itmo.example
import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.Button
import androidx.appcompat.app.AppCompatActivity

class MainActivity : AppCompatActivity() {
    private var startTARImplButton: Button? = null
    private var startRXJavaImplButton: Button? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        startTARImplButton = findViewById(R.id.TARImpl)
        startRXJavaImplButton = findViewById(R.id.RXJavaImpl)
    }

    fun startTARImpl(view: View) {
        startTARImplButton?.isEnabled = false
        startActivity(Intent(this, ThreadAndRunnableImplementation::class.java))
        startTARImplButton?.isEnabled = true
    }

    fun startRXJavaImpl(view: View) {
        startRXJavaImplButton?.isEnabled = false
        startActivity(Intent(this, RXJavaImplementation::class.java))
        startRXJavaImplButton?.isEnabled = true
    }
}