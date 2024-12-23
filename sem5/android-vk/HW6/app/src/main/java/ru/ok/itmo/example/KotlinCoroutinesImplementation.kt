package ru.ok.itmo.example
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.ProgressBar
import androidx.appcompat.app.AppCompatActivity
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.MainScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

class KotlinCoroutinesImplementation : AppCompatActivity() {
    private var progressBar: ProgressBar? = null
    private var startButton: Button? = null
    private var job: Job? = null
    private val mainScope = MainScope()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.button_and_progress_bar)

        progressBar = findViewById(R.id.progressBar)
        startButton = findViewById(R.id.startButton)
    }

    fun startProgressBar(view: View) {
        startButton?.isEnabled = false

        job = mainScope.launch(Dispatchers.IO) {
            for (i in 0 until 100) {
                try {
                    Thread.sleep(100)
                    withContext(Dispatchers.Main) {
                        progressBar?.progress = i + 1
                    }
                } catch (e: InterruptedException) {
                    e.printStackTrace()
                }
            }
            withContext(Dispatchers.Main) {
                startButton?.isEnabled = true
            }
        }
    }

    override fun onDestroy() {
        super.onDestroy()
        job?.cancel()
    }
}