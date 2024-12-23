package ru.ok.itmo.example

import android.content.res.Configuration
import android.os.Bundle
import android.text.InputType
import android.view.KeyEvent
import android.view.inputmethod.EditorInfo
import android.widget.Button
import android.widget.CheckBox
import android.widget.EditText
import android.widget.TextView
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import androidx.appcompat.app.AppCompatDelegate

class MainActivity : AppCompatActivity() {
    companion object {
        const val MIN_PASSWORD_LENGTH = 6
    }

    private lateinit var editTextEmail: EditText
    private lateinit var editTextPassword: EditText
    private lateinit var buttonLogin: Button
    private lateinit var checkBoxShowPassword: CheckBox
    private lateinit var themeToggleButton: Button
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        editTextEmail = findViewById(R.id.editTextEmail)
        editTextPassword = findViewById(R.id.editTextPassword)
        buttonLogin = findViewById(R.id.buttonLogin)
        checkBoxShowPassword = findViewById(R.id.checkBoxShowPassword)
        themeToggleButton = findViewById(R.id.themeToggleButton)

        buttonLogin.setOnClickListener {
            performLogin()
        }

        themeToggleButton.setOnClickListener {
            toggleTheme()
        }

        editTextPassword.setOnEditorActionListener(TextView.OnEditorActionListener { _, actionId, event ->
            if (actionId == EditorInfo.IME_ACTION_DONE ||
                (event != null && event.action == KeyEvent.ACTION_DOWN &&
                        event.keyCode == KeyEvent.KEYCODE_ENTER)
            ) {
                performLogin()
                return@OnEditorActionListener true
            }
            false
        })

        checkBoxShowPassword.setOnCheckedChangeListener { _, isChecked ->
            if (isChecked) {
                editTextPassword.inputType = InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD
            } else {
                editTextPassword.inputType =
                    InputType.TYPE_CLASS_TEXT or InputType.TYPE_TEXT_VARIATION_PASSWORD
            }
        }
    }

    private fun performLogin() {
        val email = editTextEmail.text.toString()
        val password = editTextPassword.text.toString()

        if (email.isEmpty() || password.isEmpty()) {
            showToast(R.string.error_empty_fields)
            return
        }

        if (!isEmailValid(email)) {
            showToast(R.string.error_invalid_email)
            return
        }


        if (password.length < MIN_PASSWORD_LENGTH) {
            showToast(R.string.error_short_password, MIN_PASSWORD_LENGTH)
            return
        }

        showToast(R.string.success_authorization)
    }

    private fun isEmailValid(email: String): Boolean {
        return android.util.Patterns.EMAIL_ADDRESS.matcher(email).matches()
    }

    private fun showToast(messageResId: Int, vararg formatStr: Any) {
        Toast.makeText(
            this,
            getString(messageResId, *formatStr),
            Toast.LENGTH_SHORT
        ).show()
    }

    private fun toggleTheme() {
        val newNightMode =
            when (resources.configuration.uiMode and Configuration.UI_MODE_NIGHT_MASK) {
                Configuration.UI_MODE_NIGHT_NO -> AppCompatDelegate.MODE_NIGHT_YES
                Configuration.UI_MODE_NIGHT_YES -> AppCompatDelegate.MODE_NIGHT_NO
                else -> AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM
            }

        AppCompatDelegate.setDefaultNightMode(newNightMode)
        delegate.applyDayNight()
    }
}
