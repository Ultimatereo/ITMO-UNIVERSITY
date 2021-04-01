public class SumLong{
	public static void main(String[] args){
		long s = 0L;
		String str="";
		for (int i = 0; i<args.length; i++){
			try{
				s+=Long.parseLong(args[i]);
			}catch(NumberFormatException e){

				char[] chars = args[i].toCharArray();

				for (char t: chars){
					if (Character.isDigit(t) == false && str!=""){
						str = String.join("", str, " ");
					} if (Character.isWhitespace(t)==false && Character.toString(t).equals("?")==false){
						str = String.join("", str, Character.toString(t));
					}
					
					//System.out.println(t);
					//System.out.println(s);
				}
				//System.out.println(str);
				str += " ";
			}
		
		//String[] pp = str;
		//System.out.println(pp.length);
		
			/*
			System.out.println(args[i]);
			System.out.println(Integer.parseInt(args[i]));
			s+=Integer.parseInt(args[i]);
			*/

		}
		try{
			while(str.contains("  ")) {
    			str = str.replace("  ", " ");
    		}

			String[] strArray = str.split(" ");
			//String[] strArray = str.split("\\s+");
			for (String pp : strArray) {
				//System.out.println(pp);
        		s+=Long.parseLong(pp);
        	}
        	System.out.println(s);

        }catch(NumberFormatException e){
       		System.out.println(s);
        }
		//System.out.println(str[0]);
        //System.out.println(s);
	}
}