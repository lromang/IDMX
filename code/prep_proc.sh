#! /bin/bash


# -----------------------
# Download file
# -----------------------
# IF xlsx convert to csv
# -----------------------
wget  $1 --output-document tmp.csv

# -----------------------
# Correct encoding
# -----------------------
encode=$(file -i tmp.csv | awk -F '=' '{print $2}')
file="correct_tmp.csv"
iconv -f $encode -t utf8 tmp.csv > $file

# -----------------------
# Metadata display
# -----------------------
echo "#--------------------"             > metadata.txt
echo "Metadatos:"                        >> metadata.txt
echo "#--------------------"             >> metadata.txt
echo -e "\n"                             >> metadata.txt
echo "#--------------------"             >> metadata.txt
echo "URL: "                             >> metadata.txt
echo "#--------------------"             >> metadata.txt
echo "$1"                                >> metadata.txt
echo -e "\n"                             >> metadata.txt
echo "#--------------------"             >> metadata.txt
echo "Codificación:"                     >> metadata.txt
echo "#--------------------"             >> metadata.txt
echo "$encode"                           >> metadata.txt
echo -e "\n"                             >> metadata.txt
echo "#--------------------"             >> metadata.txt
echo "Tamaño:"                           >> metadata.txt
echo "#--------------------"             >> metadata.txt
echo "   - Bytes: $(wc -c tmp.csv | sed 's/tmp.csv//g')"      >> metadata.txt
echo "   - Caracteres: $(wc -m tmp.csv | sed 's/tmp.csv//g')" >> metadata.txt
echo "   - Palabras: $(wc -w tmp.csv | sed 's/tmp.csv//g')"   >> metadata.txt
echo "   - Líneas: $(wc -l tmp.csv | sed 's/tmp.csv//g')"     >> metadata.txt
echo -e "\n"                             >> metadata.txt

# -----------------------
# Errase incorrect file
# -----------------------
rm tmp.csv
