import java.util.*;
import java.io.*;
public class WordStatInputShingles {
	public static void main (String[] args) {
		String inputName = args[0];
		String outputName = args[1];
		int[] numbers = new int[1000000];
		String[] words = new String[1000000];
		int i, j, c;
		//String dash = "-";
		//String ap = "'";
		String currentWord, currentWord1;
		String currentWordpart1 = "";
		String currentWordpart2 = "";
		int beginIndex = 0;
		int endIndex = 0;
		int counter = 1;
		try {
			BufferedReader reader = new BufferedReader(new InputStreamReader(
				new FileInputStream(inputName), "UTF-8"));
			try {
				StringBuilder word = new StringBuilder();
				char[] buffer = new char[2048];
				
				while (true) {

					int read = reader.read(buffer);
					//System.out.println(Arrays.toString(buffer));
					//System.out.println(read);
					if (read == -1) {
						break;
					}
					i = 0;
					while (i < read) {
						//System.out.println(buffer[i] + " " + (int) buffer[i] + " " + Character.isLetter(buffer[i]));
						while (!(Character.isLetter(buffer[i]) || Character.DASH_PUNCTUATION == Character.getType(buffer[i])
							 || buffer[i] == '\''))  {
							i += 1;
							//System.out.println(buffer[i] + " " + (int) buffer[i] + " " + Character.isLetter(buffer[i]));
							if (counter == 0) {
								counter = 1;
								for (c = 0; c < currentWordpart1.length() - 2; c++) {
									currentWord1 = currentWordpart1.substring(c, c + 3);
									j = 0;
									while (true) {
										//System.out.println(words[j] + " , " + currentWord);
										if (words[j] == null) {
											words[j] = currentWord1;
											numbers[j] += 1;
											break;
										}
										if (words[j].equals(currentWord1)) {
											numbers[j] += 1;
											break;
										}
										j += 1;
									}
									System.out.println(counter);
								}
							}
							if (i == read) {
								break;
							}
						}
						if (i == read) {
							currentWordpart1 = "";
							break;
						}
						beginIndex = i;

						while (Character.isLetter(buffer[i]) || Character.DASH_PUNCTUATION == Character.getType(buffer[i]) || ((int) buffer[i]) == 39) {
							i += 1;
							//System.out.println(buffer[i] + " " + (int) buffer[i] + " " + Character.isLetter(buffer[i]));
							if (i == read) {
								break;
							}
						}

						endIndex = i;

						if (i == read) {
							if (Character.isLetter(buffer[i-1]) || Character.DASH_PUNCTUATION == Character.getType(buffer[i-1]) || ((int) buffer[i-1]) == 39) {
								currentWordpart1 = new String(buffer, beginIndex, endIndex - beginIndex).toLowerCase();
							} else {
								currentWordpart1 = "";
							}
								/*
								words[counter] = new String(buffer, beginIndex, endIndex - beginIndex).toLowerCase();
								numbers[counter] += 1;
								counter += 1;
								*/
								//System.out.println(Arrays.toString(words));
								//System.out.println(Arrays.toString(numbers));
							System.out.println("/ " + currentWordpart1 + " /");
							
							break;
						}
						else if (counter == 0) {
							//System.out.println(currentWordpart1);
							currentWordpart2 = new String(buffer, beginIndex, endIndex - beginIndex).toLowerCase();
							//System.out.println(currentWordpart2);
							currentWord = currentWordpart1 + currentWordpart2;
							//System.out.println(currentWord);
							//System.out.println("**********");

							
							
							counter = 1;
							for (c = 0; c < currentWord.length() - 2; c++) {
								currentWord1 = currentWord.substring(c, c + 3);
								j = 0;
								while (true) {
									//System.out.println(words[j] + " , " + currentWord);
									if (words[j] == null) {
										words[j] = currentWord1;
										numbers[j] += 1;
										break;
									}
									if (words[j].equals(currentWord1)) {
										numbers[j] += 1;
										break;
									}
									j += 1;
								}
							}
						//System.out.println(Arrays.toString(words));
						//System.out.println(Arrays.toString(numbers));

						} else {
							currentWord = new String(buffer, beginIndex, endIndex - beginIndex).toLowerCase();
							for (c = 0; c < currentWord.length() - 2; c++) {
								currentWord1 = currentWord.substring(c, c + 3);
								j = 0;
								while (true) {
									//System.out.println(words[j] + " , " + currentWord);
									if (words[j] == null) {
										words[j] = currentWord1;
										numbers[j] += 1;
										break;
									}
									if (words[j].equals(currentWord1)) {
										numbers[j] += 1;
										break;
									}
									j += 1;
								}
							}
						}
						//System.out.println("***************");
						//System.out.println(Arrays.toString(words));
						//System.out.println(Arrays.toString(numbers));
						//System.out.println();
						
					}
					counter = 0;

				}
				//System.out.println(word);
			} finally {
				reader.close();
			}
		} catch (IOException e) {
			System.out.println("I/O error: " + e.getMessage());
		}


		try {
			PrintWriter writer = new PrintWriter(new OutputStreamWriter(
				new FileOutputStream(outputName), "UTF-8"));
			try {
				j = 0;
				while (true) {
					if (words[j] == null) {
						break;
					}
					writer.println(words[j] + " " + numbers[j]);
					j += 1;
				}
			} finally {
				writer.close();
			}
		} catch (IOException e) {
			System.out.println("I/O error: " + e.getMessage());
		}


	}
}