package ru.ok.itmo.example

import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.ProgressBar
import android.widget.RadioButton
import android.widget.RadioGroup
import androidx.appcompat.app.AppCompatActivity
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers
import io.reactivex.rxjava3.core.Observable
import io.reactivex.rxjava3.disposables.Disposable
import io.reactivex.rxjava3.schedulers.Schedulers
import java.util.concurrent.TimeUnit

class RXJavaImplementation : AppCompatActivity() {
    private var progressBar: ProgressBar? = null
    private var startButton: Button? = null
    private var restartButton: Button? = null
    private var timeRadioGroup: RadioGroup? = null
    private var disposable: Disposable? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.button_and_progressbar_activity_jrx)

        progressBar = findViewById(R.id.progressBar)
        startButton = findViewById(R.id.startButton)
        restartButton = findViewById(R.id.restartButton)
        timeRadioGroup = findViewById(R.id.timeRadioGroup)
        timeRadioGroup!!.check(R.id.radioButton100)
    }

    fun startProgressBar(view: View) {
        startButton?.isEnabled = false
        timeRadioGroup?.isEnabled = false

        val selectedRadioButtonId = timeRadioGroup?.checkedRadioButtonId
        val selectedRadioButton = findViewById<RadioButton>(selectedRadioButtonId ?: -1)
        val interval = selectedRadioButton.text.toString().replace("ms", "").toLong()

        disposable = Observable.interval(interval, TimeUnit.MILLISECONDS)
            .take(100)
            .subscribeOn(Schedulers.io())
            .observeOn(AndroidSchedulers.mainThread())
            .subscribe { tick ->
                progressBar?.progress = (tick + 1).toInt()
                if (tick == 99L) {
                    startButton?.isEnabled = true
                    restartButton?.visibility = View.VISIBLE
                }
            }
    }

    fun restartProgressBar(view: View) {
        progressBar?.progress = 0
        restartButton?.visibility = View.GONE
    }

    override fun onDestroy() {
        super.onDestroy()
        disposable?.dispose()
    }
}
