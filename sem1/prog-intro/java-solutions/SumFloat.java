public class SumFloat {
	public static void main(String[] args) {
		float sum = 0;
		int start, end, i;
		boolean check = false;

		for (String arg : args) {
		   	//System.err.println(arg);
			i = 0;
			while (i < arg.length()) {
			    while (i != arg.length() && Character.isWhitespace(arg.charAt(i))) {
					i++;
					//System.err.println("start++ " + start + " " + end + " i: " + i);
				}

				if (i == arg.length()) {
					break;
				}
				start = i;
				//System.err.println("start changed. " + start + " " + end);

				while (i != arg.length() && !Character.isWhitespace(arg.charAt(i))) {
					check = true;
					if (i == arg.length() - 1) {
						i++;
						break;
					}
					i++;
					//System.err.println("end++ " + start + " " + end+ " i: " + i);
				}

				end = i;
				//System.err.println(start + " " + end);
				if (check) {
					sum += Float.parseFloat(arg.substring(start, end));
					/*
					Well, the API for Integer.valueOf(String) does indeed say that the String is interpreted exactly as if it were given to 
					Integer.parseInt(String). However, valueOf(String) returns a new Integer() obiect whereas parseInt(String) returns a primitive int.
					*/
				}
				check = false;
				i++;
				//System.err.println("sum changed: " + sum + " start: " + start + " end: " + end);
			}
		}
		System.out.println(sum);
	}
}