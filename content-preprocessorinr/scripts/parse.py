import email.parser
import email.utils
import sys

def parseElement(filename,element,type):
	parser = email.parser.Parser()
	email_val = parser.parse(open(filename,"r"))
	element_val=None

	if element.lower()=="message":

		if email_val.is_multipart():
			for part in email_val.walk():
				ctype = part.get_content_type()
				cdispo = str(part.get('Content-Disposition'))
				
				#skip any text/plain (txt) attachments
				if ctype == 'text/html' and 'attachment' not in cdispo:
					element_val = part.get_payload(decode=True) #decode
					break
		#not multipart -i.e. plain text, no attachments, keeping fingers crossed
		else:
			element_val = email_val.get_payload(decode = True)
	else:
		element_val=email_val.get_all(element)[0]
		
		
	if element_val!=None:
		print( element_val)
	else:
		print ("")


parseElement(sys.argv[1],sys.argv[2],sys.argv[3])
