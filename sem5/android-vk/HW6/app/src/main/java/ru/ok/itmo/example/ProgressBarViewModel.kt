package ru.ok.itmo.example
import androidx.lifecycle.ViewModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.withContext

class ProgressBarViewModel : ViewModel() {

    private val _progress = MutableStateFlow(0)
    val progress: StateFlow<Int> = _progress.asStateFlow()

    suspend fun startProgressBar() {
        withContext(Dispatchers.IO) {
            for (i in 0 until 100) {
                delay(100)
                _progress.value = i + 1
            }
        }
    }
}
