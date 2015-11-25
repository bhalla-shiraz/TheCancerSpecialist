import java.io.*;

import rcaller.RCaller;
import rcaller.RCode;

public class RProcessor {
	
	private static float acc;
	private static String diag;
	
	public  void processData()
	{
		int minIteration 			= 0;
		int maxIteration 			= 1;
		int chosenClusterIndex 		= 0;
		String[] classifiers = {"Dtree.R","Bagging.R","Boosting.R","KNN.R","logistic.R","NaiveBayes.R","neuralNet.R","RandomForest.R","SVMperceptron.R","svm.R"};
		// array 1d length = classifiers
		//each element is average of 5 runs
		float[]  avgAccuracy = new float[classifiers.length];
		for(int classifierIndex = minIteration; classifierIndex < classifiers.length; classifierIndex++){
			for(int iteration = minIteration; iteration < maxIteration; iteration++){
		
				avgAccuracy[classifierIndex] += testAllClassifiers(classifiers[classifierIndex]);
			}
			avgAccuracy[classifierIndex] = avgAccuracy[classifierIndex] / maxIteration;
		}
		
		//get the classifier with max average
		chosenClusterIndex = maxClassAvg(avgAccuracy);
		
		//process the data again on this classifier
		// TODO create a test array from the report
		finalTestClassifier(classifiers[chosenClusterIndex]);
		
	}
	
	public static float finalTestClassifier(String classifier){
		try {
    		System.out.println(classifier);
	  		  //Create an object to link java to R
	  	      RCaller caller = new RCaller();
	  	      caller.setRscriptExecutable("C:/Program Files/R/R-3.2.2/bin/Rscript");
	  	      
	  	      //creating a code object to compile in R
	  	      RCode code = new RCode();
	  	      //Clear previous code assignments to the code object
	  	      //not necessary here but should be used later on
	  	      code.clear();

	  	      //Get the code from a R code file created
	  	      code.R_source(classifier);
	  	      
	  	      //set code to the Rcaller object
	  	      caller.setRCode(code);
	  	      //get the result from return list with key = acc
	  	      caller.runAndReturnResult("acc");
	  	      
	  	      //extract the result array
	  	      double[] results = caller.getParser().getAsDoubleArray("acc");
	  	      //System.out.println("Accuracy: "+(float)(results[0]*100));
	  	      acc = (float)(results[0]*100);
	  	      
	  	    } catch (Exception e) {
	  	     e.printStackTrace();
	  	    }
		return acc;
	
		
	}
	
	public void diagnose(float[] al){
		
		try {
    		
  		  //Create an object to link java to R
  	      RCaller caller = new RCaller();
  	      caller.setRscriptExecutable("C:/Program Files/R/R-3.2.2/bin/Rscript");
  	      
  	      //creating a code object to compile in R
  	      RCode code = new RCode();
  	      //Clear previous code assignments to the code object
  	      //not necessary here but should be used later on
  	      code.clear();

  	      //go to line 36 and add line of test data
  	      String path1 = "svm1.R";
  	      String path2 = "svm2.R";
  	      String path3 = "svm3.R";
  	      
  	      //float al[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
  	      try
  	      {
  	          BufferedReader file = new BufferedReader(new FileReader(path1));
  	          String line;
  	          String input = "";
  	          while((line = file.readLine()) != null)
  	          {
  	              input += line + "\r\n";
  	          }
  	          file.close();
  	          String formula = "testData[1,1] <-";
  	          formula += Float.toString(al[0]) + "\r\n";
  	          for(int index = 1; index < al.length; index++){
  	        	  formula += "testData[1," + (index + 1) + "] <- " + Float.toString(al[index]) +"\r\n";  
  	          }
  	          
  	          input += formula;
  	          
  	          BufferedReader file2 = new BufferedReader(new FileReader(path2));
  	          String line2;
  	          //String input2 = "";
  	          while((line2 = file2.readLine()) != null)
  	          {
  	              input += line2 + "\r\n";
  	          }
  	          
  	          
  	          FileOutputStream out = new FileOutputStream(path3);
  	          out.write(input.getBytes());
  	          
  	          out.close();
  	          file2.close();
  	          
  	      }
  	      catch(Exception e)
  	      {
  	          System.out.println("Error overwriting file: " + path1);
  	          e.printStackTrace();
  	      }
  	      
  	      
  	      System.out.println("Get R");
  	    //Get the code from a R code file created
  	      code.R_source("svm3.R");
  	      
  	      //set code to the Rcaller object
  	      caller.setRCode(code);
  	      
  	      caller.runAndReturnResult("result");
  	      String[] results = caller.getParser().getAsStringArray("result");
  	      System.out.println("Mean is " + results[0]);
  	      
  	      //TODO
  	      if(results[0].equals("B"))
  	    	  diag = "Benign";
  	      else if(results[0].equals("M"))
  	    	  diag = "Malignant";
  	      System.out.println(diag);
  	      
  	      plotData();
  	    		  } catch (Exception e) {
  	     e.printStackTrace();
  	    }
	
	}

	public static int maxClassAvg(float[] avgAccuracy){
		
		int clusterIndex 		= 0;
		float maxAccAvg 		= -2;
		
		for(int cluster = 0; cluster < avgAccuracy.length; cluster++)
		{
			if(avgAccuracy[cluster] > maxAccAvg){
				maxAccAvg = avgAccuracy[cluster];
				clusterIndex = cluster;
			}
		}
		
		System.out.println(maxAccAvg);
		return clusterIndex;
		
	}
	
	public static String getDiag(){
		return(diag);
	}
	public static float getAcc(){
		
		return(acc);
		
	}
	
	public static void plotData()
	{
		 //Create an object to link java to R
	      RCaller caller = new RCaller();
	      caller.setRscriptExecutable("C:/Program Files/R/R-3.2.2/bin/Rscript");
	      
	      //creating a code object to compile in R
	      RCode code = new RCode();
	      //Clear previous code assignments to the code object
	      //not necessary here but should be used later on
	      code.clear();

	      //Get the code from a R code file created
	      code.R_source("Rplotter.R");
	      
	      //set code to the Rcaller object
	      caller.setRCode(code);
	      caller.runOnly();
	     
	}
	public static float testAllClassifiers(String file)
	{
		try {
    		System.out.println(file);
	  		  //Create an object to link java to R
	  	      RCaller caller = new RCaller();
	  	      caller.setRscriptExecutable("C:/Program Files/R/R-3.2.2/bin/Rscript");
	  	      
	  	      //creating a code object to compile in R
	  	      RCode code = new RCode();
	  	      //Clear previous code assignments to the code object
	  	      //not necessary here but should be used later on
	  	      code.clear();

	  	      //Get the code from a R code file created
	  	      code.R_source(file);
	  	      
	  	      //set code to the Rcaller object
	  	      caller.setRCode(code);
	  	      //get the result from return list with key = acc
	  	      caller.runAndReturnResult("acc");
	  	      
	  	      //extract the result array
	  	      double[] results = caller.getParser().getAsDoubleArray("acc");
	  	      System.out.println("Accuracy: "+(float)(results[0]*100));
	  	      return (float) (results[0]*100);
	  	      
	  	    } catch (Exception e) {
	  	     e.printStackTrace();
	  	    }
		return 0;
	}
	
}