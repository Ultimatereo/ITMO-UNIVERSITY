import java.util.*;
import java.io.*;
public class WordStatInput {
	public static void main (String[] args) {
		String inputName = args[0];
		String outputName = args[1];
		int[] numbers = new int[100001];
		String[] words = new String[100001];
		int i, j, beginIndex, endIndex;
		String dash = "-";
		String ap = "'";
		String currentWord;
		try {
			BufferedReader reader = new BufferedReader(new InputStreamReader(
				new FileInputStream(inputName), "UTF-8"));
			try {
				StringBuilder word = new StringBuilder();
				char[] buffer = new char[100001];
				//System.out.println(Arrays.toString(buffer));
				while (true) {
					int read = reader.read(buffer);
					//System.out.println(read);
					if (read == -1) {
						break;
					}
					i = 0;
					while (i < read) {
						//System.out.println(buffer[i] + " " + (int) buffer[i] + " " + Character.isLetter(buffer[i]));
						while (!(Character.isLetter(buffer[i]) || Character.DASH_PUNCTUATION == Character.getType(buffer[i]) || ((int) buffer[i]) == 39))  {
							i += 1;
							//System.out.println(buffer[i] + " " + (int) buffer[i] + " " + Character.isLetter(buffer[i]));
							if (i == read) {
								break;
							}
						}
						if (i == read) {
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
							if (Character.isLetter(buffer[i - 1]) || Character.DASH_PUNCTUATION == Character.getType(buffer[i-1]) || ((int) buffer[i - 1]) == 39) {
								currentWord = new String(buffer, beginIndex, endIndex - beginIndex).toLowerCase();
								j = 0;
								while (true) {
									//System.out.println(words[j] + " , " + currentWord);
									if (words[j] == null) {
										words[j] = currentWord;
										numbers[j] += 1;
										break;
									}
									if (words[j].equals(currentWord)) {
										numbers[j] += 1;
										break;
									}
									j += 1;
								}
								/*
								words[counter] = new String(buffer, beginIndex, endIndex - beginIndex).toLowerCase();
								numbers[counter] += 1;
								counter += 1;
								*/
								//System.out.println(Arrays.toString(words));
								//System.out.println(Arrays.toString(numbers));
							}
							break;
						}
						currentWord = new String(buffer, beginIndex, endIndex - beginIndex).toLowerCase();
						j = 0;
						while (true) {
							//System.out.println(words[j] + " , " + currentWord);
							if (words[j] == null) {
								words[j] = currentWord;
								numbers[j] += 1;
								break;
							}
							if (words[j].equals(currentWord)) {
								numbers[j] += 1;
								break;
							}
							j += 1;
						}
						//System.out.println("***************");
						//System.out.println(Arrays.toString(words));
						//System.out.println(Arrays.toString(numbers));
					}


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