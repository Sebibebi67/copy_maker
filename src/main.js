//==================================== main.js ===================================//


//--------------------------------- Description ----------------------------------//
//
// This file contains all methods to translate a CSV file into a COBOL Copy
//
//--------------------------------------------------------------------------------//


//----------------------------------- Authors ------------------------------------//
//
// Sébastien HERT
//
//--------------------------------------------------------------------------------//


//------------------------------------ Errors ------------------------------------//
//
const errorCSV = 'Error : the given file is not a CSV file.';
const errorBrowser = 'Error : the browser does not support "FileReader".\nPlease use another one.';
// 
//--------------------------------------------------------------------------------//


//------------------------------- Global Variables -------------------------------//
//
var csvFile;
var processOk = true;
var outputDivision;
var copy;
var copyName;
var typeColumnIndex;
var increment;
// 
//--------------------------------------------------------------------------------//


//------------------------------- Global Constants -------------------------------//
//
const regexFile = /^([a-zA-Z0-9\s_\\.\-:])+(.csv)$/;
const regexData = /^\:[a-zA-Z0-9]{6}\:\-/;
const regexType = /^[S]?[X9]+\([0-9]*\)/;
const parser = ';';

const border = ' ============================================================== *';
const cName    		= 'Nom de Copy : ';
const cVersion 		= 'Version     : ';
const cDescription 	= 'Description : ';
const cDate			= 'Date        : ';
const cAuthor		= 'Auteur      : ';
const cLength		= 'Longueur    : ';

const margin = '       ';
const commentedMargin = '      *';
const space = ' '; 
const point = '.';
const newLine = "\n";

const FILLER = 'FILLER';
const ENCODING = 'UTF-8';
const PIC = 'PIC';
// 
//--------------------------------------------------------------------------------//


//------------------------------------- Main -------------------------------------//
//

// this part contains the event Listener used to display the choosen file's name
const actualBtn = document.getElementById('CopyCSV');
const fileChosen = document.getElementById('chosenFile');
actualBtn.addEventListener('change', function(){
  fileChosen.textContent = this.files[0].name
})

/**
 * Description : Generates the COBOL Copy with the given file
 *
 * Input :
 * - None
 *
 * Output :
 * - None
 *
 * Authors :
 * - Sébastien HERT
 */
function generateCopy() {

	// Getting the output division and clearing it
	outputDivision = document.getElementById('output');
	while (outputDivision.firstChild) {
		outputDivision.firstChild.remove();
	}

	// Getting the File
	csvFile = document.getElementById("CopyCSV");

	// Getting the copy Name
	copyName = document.getElementById("CopyName").value;

	// Getting the index of the Type column
	typeColumnIndex = document.getElementById("TypeColumnIndex").value;

	// Getting the length of the increment
	increment = document.getElementById("IncrementValue").value;

	copy = new FileCopy(copyName);

	// Checking fileName
	checkFile();

	// Parsing the file
	if (processOk){ parseFile(); }
}

/**
 * Description : Allows the user to copy to keyboard the generated Copy.
 * It uses execCommand, which is deprecated, but any equivalent existed,
 * so it has been decided to leave it in.
 *
 * Input :
 * - None
 *
 * Output :
 * - None
 *
 * Authors :
 * - Sébastien HERT
 */
function copyToClipboard(){
	var range = document.createRange();
	range.selectNode(document.getElementById("output"));
	window.getSelection().removeAllRanges(); // clear current selection
	window.getSelection().addRange(range); // to select text
	document.execCommand("copy");
	window.getSelection().removeAllRanges();
}



// 
//--------------------------------------------------------------------------------//

//---------------------------------- Functions -----------------------------------//
//
/**
 * Description : Verifies if the file is a CSV
 *
 * Input :
 * - None
 *
 * Output :
 * - None
 *
 * Authors :
 * - Sébastien HERT
 */
function checkFile() {
	if (!regexFile.test(csvFile.value.toLowerCase())){
		displayError(errorCSV);
	}

	if (typeof (FileReader) == "undefined") {
		displayError(errorBrowser);
	}
}

/**
 * Description : Parses the file into lines
 *
 * Input :
 * - None
 *
 * Output :
 * - None
 *
 * Authors :
 * - Sébastien HERT
 */
function parseFile(){
	var reader = new FileReader();

	reader.onload = function(e){
		var content = reader.result;
		var lines = content.split(newLine);

		// For each line
		for (var count = 0; count < lines.length; count++) {
			copy.addLine(lines[count]);
		}
		displayOutput();	
	}
	reader.readAsText(csvFile.files[0], ENCODING);
}

/**
 * Description : Fills the output Division
 *
 * Input :
 * - None
 *
 * Output :
 * - None
 *
 * Authors :
 * - Sébastien HERT
 */
function displayOutput(){
	outputDivision.className = 'output-visible';
	outputDivision.appendChild(copy.generateHeader());
	outputDivision.appendChild(document.createElement("br"));
	copy.lines.forEach(line => {
		outputDivision.appendChild(line.generateLine());
		outputDivision.appendChild(document.createElement("br"));
	});
}

/**
 * Description : Displays an error on the console and on the output
 *
 * Input :
 * - error : the error as a String
 *
 * Output :
 * - None
 *
 * Authors :
 * - Sébastien HERT
 */
function displayError(error){
	console.error(error);
	processOk = false;
}
// 
//--------------------------------------------------------------------------------//


//================================ Class FileCopy ================================//

//--------------------------------- Description ----------------------------------//
//
// Contains the structure of the COBOL Copy File
//
//--------------------------------------------------------------------------------//

class FileCopy {

	copyName = 'Yxxxxxx';
	copyNameAsParameter;
	version = 'V(1.00)';
	description = '';
	length = '100';
	author = '';
	date = 'JJ/MM/AAAA';
	previousIndent = '';

	lines = [];


//--------------------------------- Constructor ----------------------------------//

	constructor(coryName){
		if (copyName.length == 7){
			this.copyName = copyName.toUpperCase();
		}
		this.copyNameAsParameter = ':' + this.copyName.substring(1,7) + ':';

		this.date = this.getDate()
	}

//--------------------------------------------------------------------------------//

	/**
	 * Description : Adds a line to the Copy
	 *
	 * Input :
	 * - the line as a String
	 *
	 * Output :
	 * - None
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	addLine (lineAsString) {
		// Parsing the line
		var parameters = lineAsString.split(parser);
		// console.log(parameters);
		if (parameters.length > 1){
			var displayName = "";
			if (parameters[1]?.trim() == "" || parameters[1]?.trim() == FILLER){
				displayName = FILLER;
			}else{
				displayName = this.copyNameAsParameter + '-' + this.cleanData(parameters[1]);
			}

			// Creating a new Line		
			var line = new Line(
				parameters[0]?.trim(),
				displayName,
				this.cleanType(parameters[2]),
				this.cleanDescription(parameters[3]),
				this.previousIndent);
			
			// Preventing indentation after reading a level 88 or 77
			if (line.level != '88' && line.level != '77'){
				this.previousIndent = line.indent;
			}
	
			// Adding the line to the list
			this.lines.push(line);
		}


	}

	/**
	 * Description : Generates the copy header
	 *
	 * Input :
	 * - None
	 *
	 * Output :
	 * - The HTML element which contains the header
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	generateHeader(){
		var header = document.createElement("div")

		var commentedLine1 = document.createElement("span");
		commentedLine1.className = 'comment';
		commentedLine1.textContent = commentedMargin
									+ border;

		var copyNameLine = document.createElement("span");
		copyNameLine.className = 'comment';
		copyNameLine.textContent = commentedMargin
								 + space
								 + cName
								 + this.copyName;

		var versionLine = document.createElement("span");
		versionLine.className = 'comment';
		versionLine.textContent = commentedMargin
								+ space
								+ cVersion
								+ this.version;	

		var descriptionLine = document.createElement("span");
		descriptionLine.className = 'comment';
		descriptionLine.textContent = commentedMargin
									+ space
									+ cDescription
									+ this.description;

		var dateLine = document.createElement("span");
		dateLine.className = 'comment';
		dateLine.textContent = commentedMargin
							 + space	
							 + cDate
							 + this.date;

		var authorLine = document.createElement("span");
		authorLine.className = 'comment';
		authorLine.textContent = commentedMargin
							   + space
							   + cAuthor
							   + this.author;

		var lengthLine = document.createElement("span");
		lengthLine.className = 'comment';
		lengthLine.textContent = commentedMargin
							   + space
							   + cLength
							   + this.length;

		var firstLine = document.createElement("span");
		firstLine.className = 'CopyLine';
		firstLine.textContent = margin
							  + '01'
							  + space
							  + space
							  + this.copyNameAsParameter
							  + point;

		var commentedLine2 = document.createElement("span");
		commentedLine2.className = 'comment';
		commentedLine2.textContent = commentedMargin
									+ border;

		header.appendChild(commentedLine1);
		header.appendChild(document.createElement("br"));
		header.appendChild(copyNameLine);
		header.appendChild(document.createElement("br"));
		header.appendChild(versionLine);
		header.appendChild(document.createElement("br"));
		header.appendChild(descriptionLine);
		header.appendChild(document.createElement("br"));
		header.appendChild(dateLine);
		header.appendChild(document.createElement("br"));
		header.appendChild(authorLine);
		header.appendChild(document.createElement("br"));
		header.appendChild(lengthLine);
		header.appendChild(document.createElement("br"));
		header.appendChild(commentedLine2);
		header.appendChild(document.createElement("br"));
		header.appendChild(document.createElement("br"));
		header.appendChild(firstLine);

		return header;
	}

	/**
	 * Description : Calculates the current date with the DD/MM/YYYY format
	 *
	 * Input :
	 * - None
	 *
	 * Output :
	 * - the current date as a string with the DD/MM/YYYY foramat
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	getDate(){
		var today = new Date();
		var dd = String(today.getDate()).padStart(2, '0');
		var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
		var yyyy = today.getFullYear();
		
		today = dd + '/' + mm + '/' + yyyy;
		return today;
	}


	/**
	 * Description : Cleans the data input field
	 *
	 * Input :
	 * - data : the data field as a String
	 *
	 * Output :
	 * - CleanedDta : the cleaned data field as a String
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	cleanData(data){

		// if our data isn't a String -> let's do nothing
		if (!data === String){ return ''; }

		// Let's trim our data
		var cleanedData = data.trim();

	    // console.log(cleanedData);
		// And remove all potential ':xxxxxx:-'
		if (regexData.test(cleanedData)){
			cleanedData = cleanedData.replace(regexData, '');
		}

		return cleanedData;
	}

	/**
	 * Description : Cleans the type input field
	 *
	 * Input :
	 * - type : the type field as a String
	 *
	 * Output :
	 * - CleanedType : the cleaned type field as a String
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	cleanType(type){
		if (!type === String){
			return '';
		}

		// Let's trim our type
		var cleanedType = type.replace(".", "").trim();

		// if the type doesn't contain 'PIC' and needs it
		if (regexType.test(cleanedType)){
			cleanedType = PIC + space + cleanedType;
		}
		return cleanedType;
	}

	/**
	 * Description : Cleans the description input field
	 *
	 * Input :
	 * - description
	 *
	 * Output :
	 * - CleanedDescription
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	cleanDescription(description){
		if (!description === String){
			return '';
		}

		// Removing unnecessary spaces
		var cleanedDescription = description.trim();
		cleanedDescription = cleanedDescription.replace(/\s\s+/g, ' ');

		return cleanedDescription;
	}
}

//================================== Class Line ==================================//


//--------------------------------- Description ----------------------------------//
//
// Contains the description of a line
//
//--------------------------------------------------------------------------------//

class Line {
	level;
	name;
	type;
	description;
	indent = '';
	previousIndent= '';

//--------------------------------- Constructor ----------------------------------//

	constructor(level, name, type, description, previousIndent = ''){
		this.level = level;
		this.name = name;
		this.type = type;
		this.description = description;
		this.previousIndent = previousIndent;
		this.indent += this.getIndent();
	}

//--------------------------------------------------------------------------------//

	/**
	 * Description : Generates both description and data lines
	 *
	 * Input :
	 * - None
	 *
	 * Output :
	 * - the HTML division which contains both lines
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	generateLine(){
		var smallDiv = document.createElement("div")
		var lineDescription = document.createElement("span");
		lineDescription.className = 'comment'; 
		var words = this.description.split(space);
		var descriptionLines = [];
		descriptionLines.push(this.indent);
		words.forEach(word => {
			if (descriptionLines[descriptionLines.length - 1].length + word.length +1 <= 65){
				descriptionLines[descriptionLines.length - 1] += word + space;
			}
			else {
				descriptionLines[descriptionLines.length - 1] += newLine;
				descriptionLines.push(increment + word + space);
			}
		});

		descriptionLines.forEach(line => {
			lineDescription.textContent += commentedMargin
										 + line
		});
 
		// lineDescription.textContent = commentedMargin
		// 							+ this.indent
		// 							+ this.description;


		var lineContent = document.createElement("span");
		lineContent.className = 'copyLine';
		lineContent.textContent = margin
								+ this.indent
								+ this.level
								+ space
								+ this.name
								+ this.getSpaces()
								+ this.type
								+ point;

		// On en la ajouter la description que si elle est non vide
		if (this.description != ''){
			smallDiv.appendChild(lineDescription);
			smallDiv.appendChild(document.createElement("br"));
		}
		smallDiv.appendChild(lineContent);

		return smallDiv;
	}

	/**
	 * Description : Calculates the current indent
	 *
	 * Input :
	 * - None
	 *
	 * Output :
	 * - the indent as a String of spaces
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	getIndent(){
		switch (this.level) {
			case '05':
				return increment
			case '10':
				return increment + increment 
			case '15':
				return increment + increment + increment
			case '20':
				return increment + increment + increment + increment
			case '77':
			case '88':
				return this.previousIndent + increment;
			default:
				return '';
		}
	}

	/**
	 * Description : generates spaces between the parameter name and it's type/value
	 *
	 * Input :
	 * - None
	 *
	 * Output :
	 * - multiple spaces
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	getSpaces(){
		if (this.type == ''){
			return '';
		}
 

		var spaces = ''
		var currentLineLength = margin.length	
							  + this.indent.length
							  + this.level.length
							  + 1
							  + this.name.length;


		var length = typeColumnIndex - 1 - currentLineLength;
		console.log("typeColumnIndex : " + typeColumnIndex);
		console.log("length : " + length);

		if (length > 0){
			for (let index = 0; index < length; index++) {
				spaces = spaces + space;			
			}
		}
		else {
			spaces += newLine;
			for (let index = 0; index < typeColumnIndex - 1; index++) {
				spaces = spaces + space;			
			}
		}

		return spaces;
	}
}

//================================================================================//
