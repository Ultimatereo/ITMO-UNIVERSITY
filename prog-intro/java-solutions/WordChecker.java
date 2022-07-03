public class WordChecker implements Checker {
    @Override
    public boolean isCharacterValid(char c) {
        return Character.isLetter(c) ||
                Character.getType(c) == Character.DASH_PUNCTUATION ||
                c == '\'';
    }
}
