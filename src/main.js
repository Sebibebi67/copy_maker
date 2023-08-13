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
var isMessage;
// 
//--------------------------------------------------------------------------------//


//------------------------------- Global Constants -------------------------------//
//
const regexFile = /^([a-zA-Z0-9\s_\\.\-:])+(.csv)$/;
const regexData = /^\:[a-zA-Z0-9]{6}\:\-/;
const regexType = /^[S]?[X9]+\([0-9]*\)/;
const regexTypeString = /^\s*PIC\s*X+\([0-9]*\)/;
const regexTypeInteger = /^\s*PIC\s*9+\([0-9]*\)/;
const regexTypeRedefines = /^\s*REDIFINES\s*[a-zA-Z0-9:-]*/;
const regexTypeRedefinesOrigin = /\:[a-zA-Z0-9]*\:\-/;
const parser = ';';

const border = ' ============================================================== *';
const messageBorder = ' -------------------------------------------------------------- *';

const cName = 'Nom de Copy : ';
const cVersion = 'Version     : ';
const cDescription = 'Description : ';
const cDate = 'Date        : ';
const cAuthor = 'Auteur      : ';
const cLength = 'Longueur    : ';

const messageHeader1 = 'En-tête généralisé pour tout COPY de type MESSAGE          C14 *';
const messageHeader2 = 'Comportant TAGs (ou BALISEs) standards/normalisés apposées via *';
const messageHeader3 = 'commentaires standards à respecter                             *';
const messageHeader4 = ' - [MsgIdt] : identification du message (=nom COPY associé)    *';
const messageHeader5 = ' - [MsgVrs] : version en cours de la description de cette      *';
const messageHeader6 = '               partie du message                               *';
const messageHeader7 = ' - [MsgLen] : Longueur effective de cette description          *';
const messageData = 'Données métier';
const messageFooter1 = 'Eye-Catcher - balise de fin de la description, permettant de   *';
const messageFooter2 = '            vérifier à minima la cohérence du contenu porté.   *';
const messageFooter3 = '            Si balise de fin présente et non altérée...        *';
const messageDescription = '[Dsc]';
const messageConstant = '[Cst]';
const messageType = '[Typ]';
const messageIdt = '[MsgIdt]';
const messageVersion = '[MsgVrs]';
const messageLength = '[MsgLen]';
const messageIdtEnd = '[MsgIdtEnd]';
const messageString = 'String';
const messageInteger = 'Integer';

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
actualBtn.addEventListener('change', function () {
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

	// Is the copy a Message copy ?
	isMessage = document.querySelector('input[name="Message"]:checked').value == "YES";

	copy = new FileCopy(copyName);

	// Checking fileName
	checkFile();

	// Parsing the file
	if (processOk) { parseFile(); }
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
function copyToClipboard() {
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
	if (!regexFile.test(csvFile.value.toLowerCase())) {
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
function parseFile() {
	var reader = new FileReader();

	reader.onload = function (e) {
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
function displayOutput() {
	outputDivision.className = 'output-visible';
	outputDivision.appendChild(copy.generateHeader());
	if (isMessage) {
		outputDivision.appendChild(copy.generateHeaderMessage());
	}
	copy.lines.forEach(line => {
		outputDivision.appendChild(line.generateLine());
		outputDivision.appendChild(document.createElement("br"));
	});
	if (isMessage) {
		outputDivision.appendChild(copy.generateFooterMessage());
	}
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
function displayError(error) {
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

	constructor(coryName) {
		if (copyName.length == 7) {
			this.copyName = copyName.toUpperCase();
		}
		this.copyNameAsParameter = ':' + (isMessage ? this.copyName : this.copyName.substring(1, 7)) + ':';

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
	addLine(lineAsString) {
		// Parsing the line
		var parameters = lineAsString.split(parser);
		if (parameters.length > 1) {
			// the line is empty, let's do nothing
			if (parameters[0] == "" && parameters[1] == "" && parameters[2] == "" && parameters[3] == ""){
				return;
			}
			var displayName = "";
			if (parameters[1]?.trim() == "" || parameters[1]?.trim() == FILLER) {
				displayName = FILLER;
			} else {
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
			if (line.level != '88' && line.level != '77') {
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
	generateHeader() {
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
		if (!isMessage) {
			header.appendChild(document.createElement("br"));
			header.appendChild(document.createElement("br"));
		}
		return header;
	}

	/**
	 * Description : Generates the message part of the copy's header
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
	generateHeaderMessage() {
		var header = document.createElement("div")

		var commentedLine1 = document.createElement("span");
		commentedLine1.className = 'comment';
		commentedLine1.textContent = commentedMargin
			+ messageBorder;
		var commentedMessageHeader = document.createElement("span");
		commentedMessageHeader.className = 'comment';
		commentedMessageHeader.textContent = commentedMargin
			+ space
			+ messageHeader1
			+ newLine
			+ commentedMargin
			+ space
			+ messageHeader2
			+ newLine
			+ commentedMargin
			+ space
			+ messageHeader3
			+ newLine
			+ commentedMargin
			+ space
			+ messageHeader4
			+ newLine
			+ commentedMargin
			+ space
			+ messageHeader5
			+ newLine
			+ commentedMargin
			+ space
			+ messageHeader6
			+ newLine
			+ commentedMargin
			+ space
			+ messageHeader7;
		var commentedLine2 = document.createElement("span");
		commentedLine2.className = 'comment';
		commentedLine2.textContent = commentedMargin
			+ messageBorder;

		var messageHeaderLevel = document.createElement("span");
		messageHeaderLevel.className = 'CopyLine';
		messageHeaderLevel.textContent = margin
			+ increment
			+ "05"
			+ point;

		var messageIdDesc = document.createElement("span");
		messageIdDesc.className = 'comment';
		messageIdDesc.textContent = commentedMargin
			+ messageDescription
			+ "Indentifiant de cette description";
		var messageIdCst = document.createElement("span");
		messageIdCst.className = 'comment';
		messageIdCst.textContent = commentedMargin
			+ messageConstant;
		var messageIdType = document.createElement("span");
		messageIdType.className = 'comment';
		messageIdType.textContent = commentedMargin
			+ messageType
			+ messageString;
		var messageIdComplement = document.createElement("span");
		messageIdComplement.className = 'comment';
		messageIdComplement.textContent = commentedMargin
			+ messageIdt;
		var messageIdData = document.createElement("span");
		messageIdData.className = 'CopyLine';
		var tmpLine = margin + increment + increment + "10";
		messageIdData.textContent = tmpLine
			+ this.getSpacesHeader()
			+ "PIC X(008) VALUE '"
			+ copyName
			+ space
			+ "'."

		var messageIdDesc = document.createElement("span");
		messageIdDesc.className = 'comment';
		messageIdDesc.textContent = commentedMargin
			+ messageDescription
			+ "Indentifiant de cette description";
		var messageIdCst = document.createElement("span");
		messageIdCst.className = 'comment';
		messageIdCst.textContent = commentedMargin
			+ messageConstant;
		var messageIdType = document.createElement("span");
		messageIdType.className = 'comment';
		messageIdType.textContent = commentedMargin
			+ messageType
			+ messageString;
		var messageIdComplement = document.createElement("span");
		messageIdComplement.className = 'comment';
		messageIdComplement.textContent = commentedMargin
			+ messageIdt;
		var messageIdData = document.createElement("span");
		messageIdData.className = 'CopyLine';
		var tmpLine = margin + increment + increment + "10";
		messageIdData.textContent = tmpLine
			+ this.getSpacesHeader()
			+ "PIC X(008) VALUE '"
			+ copyName
			+ space
			+ "'."

		var messageVersionDesc = document.createElement("span");
		messageVersionDesc.className = 'comment';
		messageVersionDesc.textContent = commentedMargin
			+ messageDescription
			+ "Version courante de cette description";
		var messageVersionCst = document.createElement("span");
		messageVersionCst.className = 'comment';
		messageVersionCst.textContent = commentedMargin
			+ messageConstant;
		var messageVersionType = document.createElement("span");
		messageVersionType.className = 'comment';
		messageVersionType.textContent = commentedMargin
			+ messageType
			+ messageString;
		var messageVersionComplement = document.createElement("span");
		messageVersionComplement.className = 'comment';
		messageVersionComplement.textContent = commentedMargin
			+ messageVersion;
		var messageVersionData = document.createElement("span");
		messageVersionData.className = 'CopyLine';
		var tmpLine = margin + increment + increment + "10";
		messageVersionData.textContent = tmpLine
			+ this.getSpacesHeader()
			+ "PIC X(003) VALUE '"
			+ "001"
			+ "'."

		var messageLengthDesc = document.createElement("span");
		messageLengthDesc.className = 'comment';
		messageLengthDesc.textContent = commentedMargin
			+ messageDescription
			+ "Longueur effective de cette description";
		var messageLengthCst = document.createElement("span");
		messageLengthCst.className = 'comment';
		messageLengthCst.textContent = commentedMargin
			+ messageConstant;
		var messageLengthType = document.createElement("span");
		messageLengthType.className = 'comment';
		messageLengthType.textContent = commentedMargin
			+ messageType
			+ messageString;
		var messageLengthComplement = document.createElement("span");
		messageLengthComplement.className = 'comment';
		messageLengthComplement.textContent = commentedMargin
			+ messageLength;
		var messageLengthData = document.createElement("span");
		messageLengthData.className = 'CopyLine';
		var tmpLine = margin + increment + increment + "10";
		messageLengthData.textContent = tmpLine
			+ this.getSpacesHeader()
			+ "PIC 9(005) VALUE "
			+ "00100"
			+ "."

		var commentedLine3 = document.createElement("span");
		commentedLine3.className = 'comment';
		commentedLine3.textContent = commentedMargin
			+ messageBorder;
		var commentedMessageData = document.createElement("span");
		commentedMessageData.className = 'comment';
		commentedMessageData.textContent = commentedMargin
			+ space
			+ messageData;
		var commentedLine4 = document.createElement("span");
		commentedLine4.className = 'comment';
		commentedLine4.textContent = commentedMargin
			+ messageBorder;

		header.appendChild(document.createElement("br"));
		header.appendChild(commentedLine1);
		header.appendChild(document.createElement("br"));
		header.appendChild(commentedMessageHeader);
		header.appendChild(document.createElement("br"));
		header.appendChild(commentedLine2);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageHeaderLevel);
		header.appendChild(document.createElement("br"));
		header.appendChild(document.createElement("br"));
		header.appendChild(messageIdDesc);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageIdCst);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageIdType);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageIdComplement);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageIdData);
		header.appendChild(document.createElement("br"));
		header.appendChild(document.createElement("br"));
		header.appendChild(messageVersionDesc);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageVersionCst);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageVersionType);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageVersionComplement);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageVersionData);
		header.appendChild(document.createElement("br"));
		header.appendChild(document.createElement("br"));
		header.appendChild(messageLengthDesc);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageLengthCst);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageLengthType);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageLengthComplement);
		header.appendChild(document.createElement("br"));
		header.appendChild(messageLengthData);
		header.appendChild(document.createElement("br"));
		header.appendChild(document.createElement("br"));
		header.appendChild(commentedLine3);
		header.appendChild(document.createElement("br"));
		header.appendChild(commentedMessageData);
		header.appendChild(document.createElement("br"));
		header.appendChild(commentedLine4);
		header.appendChild(document.createElement("br"));
		header.appendChild(document.createElement("br"));
		return header;
	}

	/**
	 * Description : Generates the message part of the copy's footer
	 *
	 * Input :
	 * - None
	 *
	 * Output :
	 * - The HTML element which contains the footer
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	generateFooterMessage() {
		var footer = document.createElement("div")

		var commentedLine1 = document.createElement("span");
		commentedLine1.className = 'comment';
		commentedLine1.textContent = commentedMargin
			+ messageBorder;
		var commentedMessageHeader = document.createElement("span");
		commentedMessageHeader.className = 'comment';
		commentedMessageHeader.textContent = commentedMargin
			+ space
			+ messageFooter1
			+ newLine
			+ commentedMargin
			+ space
			+ messageFooter2
			+ newLine
			+ commentedMargin
			+ space
			+ messageFooter3;
		var commentedLine2 = document.createElement("span");
		commentedLine2.className = 'comment';
		commentedLine2.textContent = commentedMargin
			+ messageBorder;

		var messageFooterDesc = document.createElement("span");
		messageFooterDesc.className = 'comment';
		messageFooterDesc.textContent = commentedMargin
			+ messageDescription
			+ "Eye-Catcher lié à cette description";
		var messageFooterCst = document.createElement("span");
		messageFooterCst.className = 'comment';
		messageFooterCst.textContent = commentedMargin
			+ messageConstant;
		var messageFooterType = document.createElement("span");
		messageFooterType.className = 'comment';
		messageFooterType.textContent = commentedMargin
			+ messageType
			+ messageString;
		var messageFooterComplement = document.createElement("span");
		messageFooterComplement.className = 'comment';
		messageFooterComplement.textContent = commentedMargin
			+ messageIdtEnd;
		var messageFooterData = document.createElement("span");
		messageFooterData.className = 'CopyLine';
		var tmpLine = margin + increment + increment + "10";
		messageFooterData.textContent = tmpLine
			+ this.getSpacesHeader()
			+ "PIC X(008) VALUE '"
			+ '/'
			+ copyName
			+ "'."

		footer.appendChild(commentedLine1);
		footer.appendChild(document.createElement("br"));
		footer.appendChild(commentedMessageHeader);
		footer.appendChild(document.createElement("br"));
		footer.appendChild(commentedLine2);
		footer.appendChild(document.createElement("br"));
		footer.appendChild(document.createElement("br"));
		footer.appendChild(messageFooterDesc);
		footer.appendChild(document.createElement("br"));
		footer.appendChild(messageFooterCst);
		footer.appendChild(document.createElement("br"));
		footer.appendChild(messageFooterType);
		footer.appendChild(document.createElement("br"));
		footer.appendChild(messageFooterComplement);
		footer.appendChild(document.createElement("br"));
		footer.appendChild(messageFooterData);
		footer.appendChild(document.createElement("br"));
		footer.appendChild(document.createElement("br"));

		return footer;
	}

	/**
	 * Description : Generate the spaces needed ton align the type in the 45th column in the header when generating message copy.
	 *
	 * Input :
	 * - None
	 *
	 * Output :
	 * - a string which contains the right number of spaces.
	 *
	 * Authors :
	 * - Sébastien HERT
	 */
	getSpacesHeader() {
		var nbSpaces = 45 - 2 * increment.length - margin.length - 3;
		var spaces = '';
		for (var i = 0; i < nbSpaces; i++) {
			spaces += space;
		}
		return spaces;
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
	getDate() {
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
	cleanData(data) {

		// if our data isn't a String -> let's do nothing
		if (!data === String) { return ''; }

		// Let's trim our data
		var cleanedData = data.trim();

		// And remove all potential ':xxxxxx:-'
		if (regexData.test(cleanedData)) {
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
	cleanType(type) {
		if (!type === String) {
			return '';
		}

		// Let's trim our type
		var cleanedType = type.replace(".", "").trim();
		cleanedType = cleanedType.replace(/\s+/, " ");
		console.log(cleanedType);

		// if the type doesn't contain 'PIC' and needs it
		if (regexType.test(cleanedType)) {
			cleanedType = PIC + space + cleanedType;
		}

		// if its a REDEFINES, lets format it
		if (regexTypeRedefines.test(cleanedType)){
			cleanedType = cleanedType.replace(regexTypeRedefinesOrigin, "");
			cleanedType = cleanedType.replace(" ", space + this.copyNameAsParameter + "-");
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
	cleanDescription(description) {
		if (!description === String) {
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
	previousIndent = '';

	//--------------------------------- Constructor ----------------------------------//

	constructor(level, name, type, description, previousIndent = '') {
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
	generateLine() {
		var smallDiv = document.createElement("div")
		var lineDescription = document.createElement("span");
		lineDescription.className = 'comment';
		var words = this.description.split(space);
		var descriptionLines = [];
		if (isMessage) {
			descriptionLines.push(messageDescription);
		} else {
			descriptionLines.push(this.indent);
		}
		words.forEach(word => {
			if (descriptionLines[descriptionLines.length - 1].length + word.length + 1 <= 65) {
				descriptionLines[descriptionLines.length - 1] += word + space;
			}
			else {
				descriptionLines[descriptionLines.length - 1] += newLine;
				descriptionLines.push((isMessage ? '' : increment) + word + space);
			}
		});

		descriptionLines.forEach(line => {
			lineDescription.textContent += commentedMargin
				+ line
		});

		var typeDescription = document.createElement("span");
		typeDescription.className = 'comment';
		if (this.name == FILLER) {
			typeDescription.textContent = '';
		}
		else if (regexTypeString.test(this.type)) {
			typeDescription.textContent = commentedMargin + messageType + messageString;
		} else if (regexTypeInteger.test(this.type)) {
			typeDescription.textContent = commentedMargin + messageType + messageInteger;
		} else {
			typeDescription.textContent = '';
		}


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


		// We add the description only if it's not empty
		if (this.description != '') {
			smallDiv.appendChild(lineDescription);
			smallDiv.appendChild(document.createElement("br"));
		}

		// We add the type description its a message copy and if it's needed
		if (isMessage && typeDescription.textContent != '') {
			smallDiv.appendChild(typeDescription);
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
	getIndent() {
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
	getSpaces() {
		if (this.type == '') {
			return '';
		}


		var spaces = ''
		var currentLineLength = margin.length
			+ this.indent.length
			+ this.level.length
			+ 1
			+ this.name.length;

		// if it's a REDIFINES, you will push it to the left
		var length;
		if (regexTypeRedefines.test(this.type)){
			length = 41 - 1 - currentLineLength;
		}
		else{
			length = typeColumnIndex - 1 - currentLineLength;
		}

		if (length > 0) {
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
