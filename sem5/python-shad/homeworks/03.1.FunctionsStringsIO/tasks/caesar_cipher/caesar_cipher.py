def caesar_encrypt(message: str, n: int) -> str:
    """Encrypt message using caesar cipher

    :param message: message to encrypt
    :param n: shift
    :return: encrypted message
    """
    lowercase_letters = 'abcdefghijklmnopqrstuvwxyz'
    uppercase_letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    encrypted_str = ''

    for char in message:
        if char.islower():
            index = (lowercase_letters.index(char) + n) % 26
            encrypted_str += lowercase_letters[index]
        elif char.isupper():
            index = (uppercase_letters.index(char) + n) % 26
            encrypted_str += uppercase_letters[index]
        else:
            encrypted_str += char

    return encrypted_str
