package ru.ok.itmo.example
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.ProgressBar
import androidx.appcompat.app.AppCompatActivity
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.MainScope
import kotlinx.coroutines.cancel
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch

class KotlinFlowImplementation : AppCompatActivity() {
    private var progressBar: ProgressBar? = null
    private var startButton: Button? = null
    private val mainScope = MainScope()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.button_and_progress_bar)

        progressBar = findViewById(R.id.progressBar)
        startButton = findViewById(R.id.startButton)
    }

    fun startProgressBar(view: View) {
        startButton?.isEnabled = false

        val flow = flow {
            for (i in 0 until 100) {
                emit(i)
                delay(100)
            }
        }

        mainScope.launch {
            flow.onEach { progress ->
                progressBar?.progress = progress + 1
            }
                .flowOn(Dispatchers.IO)
                .collect()

            startButton?.isEnabled = true
        }
    }

    override fun onDestroy() {
        super.onDestroy()
        mainScope.cancel()
    }
}