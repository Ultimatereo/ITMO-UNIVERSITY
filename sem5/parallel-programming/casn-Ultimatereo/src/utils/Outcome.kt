package utils

sealed interface Outcome {
    data object FAIL : Outcome
    data object UNDECIDED : Outcome
    data object SUCCESS : Outcome

}