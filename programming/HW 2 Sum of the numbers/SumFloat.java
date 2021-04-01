public class SumFloat{
	public static void main(String[] args){
		float s = 0f;
		int i = 0, beginIndex = 0, endIndex = 0;
		float number;
  		

		for (String pp: args){
			i = 0;
			//System.out.println(pp);
			while (i < pp.length()){
				while (Character.isWhitespace(pp.charAt(i))){
					//System.out.println(pp.charAt(i));
					i += 1;
					if (i == pp.length()){
						break;
					}
				}
				
				if (i == pp.length()){
					break;
				}

				beginIndex = i;
				//System.out.println(i + " " + pp.length());
				while (!Character.isWhitespace(pp.charAt(i))){
					//System.out.println(pp.charAt(i));
					i += 1;
					if (i == pp.length()){
						break;
					}
				}
				endIndex = i;

				if (i == pp.length()){
					if (Character.isDigit(pp.charAt(i-1))){
						//System.out.println(beginIndex + " " + endIndex);
						number = Float.parseFloat(pp.substring(beginIndex, endIndex));
						s += number;
						//System.out.println(s);
					}
					break;
				}

				
				//System.out.println(beginIndex + " " + endIndex);
				number = Float.parseFloat(pp.substring(beginIndex, endIndex));
				s += number;
				//System.out.println(s);
			}
		}
			
		System.out.println(s);
			
		


	}
}