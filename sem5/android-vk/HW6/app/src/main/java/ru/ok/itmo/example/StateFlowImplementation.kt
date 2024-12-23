package ru.ok.itmo.example

import android.os.Bundle
import android.widget.Button
import android.widget.ProgressBar
import androidx.appcompat.app.AppCompatActivity
import androidx.activity.viewModels
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.lifecycleScope
import androidx.lifecycle.repeatOnLifecycle
import kotlinx.coroutines.launch

class StateFlowImplementation : AppCompatActivity() {
    private var progressBar: ProgressBar? = null
    private var startButton: Button? = null
    private val viewModel: ProgressBarViewModel by viewModels()
    private var isRunning = false

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.button_and_progress_bar)

        progressBar = findViewById(R.id.progressBar)
        startButton = findViewById(R.id.startButton)

        startButton?.setOnClickListener {
            if (!isRunning) {
                isRunning = true
                startButton?.isEnabled = false
                progressBar?.progress = 0

                lifecycleScope.launch {
                    repeatOnLifecycle(Lifecycle.State.RESUMED) {
                        viewModel.startProgressBar()
                    }
                }

                lifecycleScope.launch {
                    repeatOnLifecycle(Lifecycle.State.RESUMED) {
                        viewModel.progress.collect { progress ->
                            progressBar?.progress = progress
                            if (progress == 100) {
                                isRunning = false
                                startButton?.isEnabled = true
                            }
                        }
                    }
                }
            }
        }
    }
}