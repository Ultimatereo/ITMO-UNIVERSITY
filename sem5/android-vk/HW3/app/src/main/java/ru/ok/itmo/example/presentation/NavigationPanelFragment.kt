package ru.ok.itmo.example.presentation

import android.os.Bundle
import android.view.Menu
import android.view.View
import androidx.fragment.app.Fragment
import androidx.fragment.app.activityViewModels
import androidx.fragment.app.commit
import com.google.android.material.bottomnavigation.BottomNavigationView
import ru.ok.itmo.example.R
import java.util.Random
import kotlin.properties.Delegates

class NavigationPanelFragment : Fragment(R.layout.navigation_panel) {
    private lateinit var bottomNavigationView: BottomNavigationView
    private lateinit var menu: Menu
    private var numSections by Delegates.notNull<Int>()
    private val viewModel: RandomNumberFragment.Companion.RandomNumberViewModel by activityViewModels()
    private var currentSection: String? = null
    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)

        bottomNavigationView = view.findViewById(R.id.navigation)
        menu = bottomNavigationView.menu
        numSections = generateRandomSectionCount()

        for (i in 0 until numSections) {
            val menuItem = menu.add(Menu.NONE, i, Menu.NONE, "Section $i")
            menuItem.setIcon(R.drawable.ic_launcher_foreground)
        }

        bottomNavigationView.setOnItemSelectedListener {
            currentSection = it.title?.toString()
            if (it.itemId > numSections || it.itemId < 0) {
                throw IllegalArgumentException("Unknown menu item id ${it.itemId}")
            }
            if (viewModel.lastPage[it.itemId] != null) {
                childFragmentManager.commit {
                    setReorderingAllowed(true)
//                    setPrimaryNavigationFragment(viewModel.lastPage[it.itemId]!!)
                    replace(
                        R.id.frame_with_nav,
                        viewModel.lastPage[it.itemId]!!
                    )
                    addToBackStack(null)
                }
            } else {
                viewModel.currentPageNumber++
                val fragment = RandomNumberFragment.createRandomNumberFragment(
                    it.itemId,
                    viewModel.currentPageNumber,
                )
                childFragmentManager.commit {
//                    bottomNavigationView.selectedItemId = it.itemId
                    setReorderingAllowed(true)
//                    setPrimaryNavigationFragment(fragment)
                    replace(
                        R.id.frame_with_nav,
                        fragment
                    )
                    addToBackStack(null)
                }
            }
            true
        }

        bottomNavigationView.selectedItemId = 0
        currentSection = bottomNavigationView.menu.getItem(0).title?.toString()
    }

    private fun generateRandomSectionCount(): Int {
        val random = Random()
        return random.nextInt(3) + 3
    }

//    override fun onDetach() {
//        super.onDetach()
//        viewModel.refresh()
//    }
}