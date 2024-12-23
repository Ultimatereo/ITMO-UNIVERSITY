package ru.ok.itmo.example.presentation

import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.TextView
import androidx.core.os.bundleOf
import androidx.fragment.app.Fragment
import androidx.fragment.app.activityViewModels
import androidx.fragment.app.commit
import androidx.lifecycle.ViewModel
import ru.ok.itmo.example.R
import java.util.Random
import java.util.SortedMap

class RandomNumberFragment : Fragment(R.layout.random_number_page) {

    private lateinit var pageNumberValueTextView: TextView
    private lateinit var numberInsideSectionValueTextView: TextView
    private lateinit var randomNumberValueTextView: TextView
    private lateinit var button: Button
    private val viewModel: RandomNumberViewModel by activityViewModels()

    companion object {
        const val ARG_PAGE_NUMBER = "pageNumber"
        const val ARG_SECTION_ID = "sectionId"

        fun createRandomNumberFragment(
            sectionId: Int, pageNumber: Int
        ): RandomNumberFragment {
            return RandomNumberFragment().apply {
                arguments = bundleOf(
                    ARG_SECTION_ID to sectionId,
                    ARG_PAGE_NUMBER to pageNumber
                )
            }
        }

        class RandomNumberViewModel : ViewModel() {
            fun refresh() {
                randomNumberMap.clear()
                lastPage.clear()
                currentPageNumber = 0
            }

            // Используем MutableMap для хранения рандомных чисел по номеру страницы
            val randomNumberMap: MutableMap<Int, SortedMap<Int, Int>> = mutableMapOf()
            val lastPage: MutableMap<Int, Fragment> = mutableMapOf()
            var currentPageNumber: Int = 0
        }
    }


    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)
        // Инициализация views
        pageNumberValueTextView = view.findViewById(R.id.page_number_value)
        numberInsideSectionValueTextView = view.findViewById(R.id.number_inside_section_value)
        randomNumberValueTextView = view.findViewById(R.id.random_number_value)
        button = view.findViewById(R.id.button)

        // Получаем номер страницы из аргументов
        val pageNumber = requireArguments().getInt(ARG_PAGE_NUMBER)

        // Генерируем или получаем рандомное число для текущей страницы и устанавливаем его в TextView
        val sectionId = requireArguments().getInt(ARG_SECTION_ID)
        viewModel.randomNumberMap.putIfAbsent(sectionId, sortedMapOf())
        val randomNumber =
            viewModel.randomNumberMap[sectionId]?.getOrPut(pageNumber) { Random().nextInt(100) }
        randomNumberValueTextView.text = randomNumber.toString()

        // Устанавливаем номер страницы и номер внутри секции в TextView
        pageNumberValueTextView.text = pageNumber.toString()

        numberInsideSectionValueTextView.text =
            viewModel.randomNumberMap[sectionId]?.size.toString()
        viewModel.lastPage[sectionId] = this
        // Обработчик нажатия на кнопку
        button.setOnClickListener {
            // Увеличиваем номер текущей страницы
            viewModel.currentPageNumber++

            // Создаем новый фрагмент для следующей страницы
            val newFragment = createRandomNumberFragment(
                requireArguments().getInt(ARG_SECTION_ID), viewModel.currentPageNumber
            )

            // Заменяем текущий фрагмент на новый
            parentFragmentManager.commit {
                setReorderingAllowed(true)
                replace(R.id.frame_with_nav, newFragment)
                addToBackStack(null)
            }
        }
    }

//    override fun onDetach() {
//        super.onDetach()
//        viewModel.randomNumberMap[requireArguments().getInt(ARG_SECTION_ID)]?.remove(requireArguments().getInt(
//            ARG_PAGE_NUMBER))
//    }
}
