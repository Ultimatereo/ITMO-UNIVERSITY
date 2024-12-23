package ru.ok.itmo.example
import android.os.Bundle
import android.os.Handler
import android.os.Looper
import android.view.View
import android.widget.Button
import android.widget.ProgressBar
import androidx.appcompat.app.AppCompatActivity

class ThreadAndRunnableImplementation : AppCompatActivity() {
    private var progressBar: ProgressBar? = null
    private var startButton: Button? = null

    private var progress = 0
    private val handler = Handler(Looper.getMainLooper())

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.button_and_progress_bar_tar)

        progressBar = findViewById(R.id.progressBar)
        startButton = findViewById(R.id.startButton)
    }

    fun startProgressBar(view: View) {
        startButton?.isEnabled = false

        Thread {
            while (progress < 100) {
                try {
                    Thread.sleep(100)
                    progress++
                    handler.post {
                        progressBar?.progress = progress
                    }
                } catch (e: InterruptedException) {
                    e.printStackTrace()
                }
            }
            handler.post {
                startButton?.isEnabled = true
            }
        }.start()
    }
}