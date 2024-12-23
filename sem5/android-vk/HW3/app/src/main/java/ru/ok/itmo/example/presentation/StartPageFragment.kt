package ru.ok.itmo.example.presentation

import android.os.Bundle
import android.view.View
import android.widget.Button
import androidx.fragment.app.Fragment
import androidx.fragment.app.commit
import ru.ok.itmo.example.R

class StartPageFragment : Fragment(R.layout.start_page) {
    private lateinit var button: Button
    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)

        button = view.findViewById(R.id.start_button)
        button.setOnClickListener {
            val fragment = NavigationPanelFragment()
            parentFragmentManager.commit {
                setPrimaryNavigationFragment(fragment)
                replace(R.id.main_layout, fragment)
                addToBackStack(null)
            }
        }
    }
}