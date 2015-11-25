import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import org.apache.pdfbox.cos.COSDocument;
import org.apache.pdfbox.pdfparser.PDFParser;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.util.PDFTextStripper;
 
public class PDFManager {
    
   private PDFParser parser;
   private PDFTextStripper pdfStripper;
   private PDDocument pdDoc ;
   private COSDocument cosDoc ;
   
   private String Text ;
   private String filePath;
   private File file;
   private static String displayArr[] = new String[34];
   public PDFManager() {
    	
    }
   public String ToText() throws IOException
   {
       this.pdfStripper = null;
       this.pdDoc = null;
       this.cosDoc = null;
       
       file = new File(filePath);
       parser = new PDFParser(new FileInputStream(file));
       
       parser.parse();
       cosDoc = parser.getDocument();
       pdfStripper = new PDFTextStripper();
       pdDoc = new PDDocument(cosDoc);
       pdDoc.getNumberOfPages();
       pdfStripper.setStartPage(1);
       pdfStripper.setEndPage(10);
       // reading text from page 1 to 10
       // if you want to get text from full pdf file use this code
       // pdfStripper.setEndPage(pdDoc.getNumberOfPages());
       
       Text = pdfStripper.getText(pdDoc);
       return Text;
   }
 
    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }
   public static String[] pdfExtract(String fileName)
   {
	   PDFManager pdfManager = new PDFManager();
       pdfManager.setFilePath(fileName);
       String file = null;
       System.out.println("extract");
	try {
		file = pdfManager.ToText();
	} catch (IOException e) {
		System.out.println("unable to read the file");
		e.printStackTrace();
	}
       
       String[] tokens = file.split(" ");
       System.out.println(tokens[12]);
       System.out.println(tokens[16]);
       displayArr[0] = tokens[12];
       displayArr[1] = tokens[16];
       int j = 2;
       float[] test = new float[32]; 
    		   for(int index = 0; index < tokens.length && j<34 ; index++)
    		   {
    			   try{
    			   //System.out.println(Float.valueOf(tokens[index]));
    			   displayArr[j] = Float.valueOf(tokens[index]).toString();
    			   if(j == 2)
    				   displayArr[j] = tokens[index].toString();
    			   test[j - 2]		 = Float.valueOf(tokens[index]);
 
    				   j++;
    		   }
    			   catch(NumberFormatException e){
    	    		   	
    	    	   }
    	   }
   
    	new RProcessor().processData();
       (new RProcessor()).diagnose(test);
       return displayArr;
   }
   
}