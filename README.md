# Hill Cipher

This racket utility ciphers simple text documents using the hill cipher method of encryption. 

## Installation

Make sure to have [racket](https://download.racket-lang.org/)  installed to run hill cipher.

To start simply run

```bash
racket
```


## Usage

Inside the racket prompt
```racket
>(enter! "final.rkt")

```

### Cipher Document
To cipher a document run: 
```racket
>(cipher-document "<plainText.txt>" "<outputFile.txt>" <n> "<key>")
```
eg
```racket
>(cipher-document "mails.txt" "c_mails.txt" 2 "hill")
```

### Decipher Document
To decipher a document run: 
```racket
>(decipher-document "<cipheredText.txt>" "<outputFile.txt>" <n> "<key>")
```
eg
```racket
>(decipher-document "c_mails.txt" "mails.txt" 2 "hill")
```

## Examples

c_mail was ciphered with n = 7 and key "This is our master key and idgad bla purple color toyota"

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)
