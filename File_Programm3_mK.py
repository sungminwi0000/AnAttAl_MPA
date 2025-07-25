# -*- coding: utf-8 -*-
"""
Created on Tue Jan 19 16:25:11 2021

@author: ySchmidtM
"""
# Das Programm soll den Inhalt des Master-Input-Files auf die entsprechenden Input-Files aufteilen
# Dafür müssen Daten erkannt und kopiert werden

# INPUT-FILE: WPS_MECH_MATERIAL_MECHANISCH
FILENAME = "Job-1.inp"    # Das Programm und die Textdatei im gleichen Verzeichnis

do_list = ['*Material, name=Al5182','*Elastic','*Plastic']    # Liste für Beginn des relevanten Bereiches
enddo_list = ['*Depvar','*Latent Heat','*User Defined Field']    # Liste für Ende des relevanten Bereiches

index_list = ['*Material, name=CuCrZr']    # Liste für Beginn des relevanten Bereiches
end_list = ['** INTERACTION PROPERTIES']    # Liste für Ende des relevanten Bereiches

def main():
    with open('WPS_MECH_MATERIAL_MECHANISCH.txt', 'w') as file: # Erstellung des Input-Files
        file.write('**')    # schreibt Inhalt
        file.write('\n')    # schreibt Zeilenumbruch
        file.write('**Bei CuCrZr ist nur Elastic relevent, Rest loeschen')
        file.write('\n')
        file.write('**')
        file.write('\n')
        for i in range(len(do_list)) and range(len(enddo_list)):    # Schleife welche über Elemente i der Listen iteriert
            with open(FILENAME, encoding="ascii") as input_file:    # Master-Input-File wird geöffnet
                lines = input_file.readlines()    # Dokument wird in einzelne Zeilen unterteilt
                for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
                    if line.startswith(do_list[i]):    # Schlagwortsuche
                        start_line = lines.index(line)    # Beginn relevanter Bereich
                    elif line.startswith(enddo_list[i]):    # Schlagwortsuche
                        end_line = lines.index(line)    # Ende relevanter Bereich
                Resulting_list = lines[start_line:end_line]    # relevanter Bereich wird in Liste gespeichert               
                file.write(''.join(Resulting_list))    # Einfügen des relevanten Bereichs                
                
        for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
            if line.startswith(index_list[0]):    # Schlagwortsuche
                start_line1 = lines.index(line)    # Beginn relevanter Bereich
            elif line.startswith(end_list[0]):    # Schlagwortsuche
                end_line1 = lines.index(line)    # Ende relevanter Bereich
        Resulting_list1 = lines[start_line1:end_line1-7]    # relevanter Bereich wird in Liste gespeichert
        file.write('**')
        file.write('\n')
        file.write(''.join(Resulting_list1))    # Einfügen des relevanten Bereichs
        file.write('**')

if __name__ == "__main__":
    main()



# INPUT-FILE: WPS_Elek_Contact
# FILENAME = "Job2D.inp"    # Das Programm und die Textdatei im gleichen Verzeichnis
ip_list = ['*Gap Conductance','*Surface Interaction, name=ELEKTRODE']    # Liste für Beginn des relevanten Bereiches
end1_list = ['*Surface Interaction, name=BLECH_NOSEP','*Surface Interaction, name=ELEK_NOSEP']    # Liste für Ende des relevanten Bereiches

i_list = ['** Interaction: ELEKTRODE_1','** Interaction: BLECH_1','** Interaction: ELEKTRODE_2']    # Liste für Beginn des relevanten Bereiches
end_list = ['SURF_BL1_EL1','SURF_BL2_BL1','SURF_BL2_EL2']    # Liste für Ende des relevanten Bereiches

def main():
    with open('WPS_Elek_Contact.txt', 'w') as file:    # Erstellung des Input-Files
        file.write('**')    # schreibt Inhalt
        file.write('\n')    # schreibt Zeilenumbruch
        file.write('** INTERACTION PROPERTIES')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('*Surface Interaction, name=BLECH')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*Gap Conductance')
        file.write('\n')
        with open(FILENAME, encoding="ascii") as input_file:    # Master-Input-File wird geöffnet
            lines = input_file.readlines()    # Dokument wird in einzelne Zeilen unterteilt
            for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
                if line.startswith(ip_list[0]):    # Schlagwortsuche
                    start_line = lines.index(line)    # Beginn relevanter Bereich
                elif line.startswith(end1_list[0]):    # Schlagwortsuche
                    end_line = lines.index(line)    # Ende relevanter Bereich
            Resulting_list1 = lines[start_line+1:end_line]    # relevanter Bereich wird in Liste gespeichert
            file.write(''.join(Resulting_list1))    # Einfügen des relevanten Bereichs
        file.write('**')
        file.write('\n')
        file.write('*GAP ELECTRICAL CONDUCTANCE, user')
        file.write('\n')
        file.write('*Surface Behavior, pressure-overclosure=HARD, no separation')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('*Surface Interaction, name=ELEKTRODE')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*Gap Conductance')
        file.write('\n')
        with open(FILENAME, encoding="ascii") as input_file:    # Master-Input-File wird geöffnet
            lines = input_file.readlines()    # Dokument wird in einzelne Zeilen unterteilt
            for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
                if line.startswith(ip_list[1]):    # Schlagwortsuche
                    start_line = lines.index(line)    # Beginn relevanter Bereich
                elif line.startswith(end1_list[1]):    # Schlagwortsuche
                    end_line = lines.index(line)    # Ende relevanter Bereich
            Resulting_list2 = lines[start_line+6:end_line]    # relevanter Bereich wird in Liste gespeichert
            file.write(''.join(Resulting_list2))    # Einfügen des relevanten Bereichs
        file.write('**')
        file.write('\n')
        file.write('*GAP ELECTRICAL CONDUCTANCE, user')
        file.write('\n')
        file.write('*Surface Behavior, pressure-overclosure=HARD, no separation')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('** INTERACTIONS')
        file.write('\n')
        file.write('**\n')
        for i in range(len(i_list)) and range(len(end_list)):    # Schleife welche über Elemente i der Listen iteriert
            with open(FILENAME, encoding="ascii") as input_file:    # Master-Input-File wird geöffnet
                lines = input_file.readlines()    # Dokument wird in einzelne Zeilen unterteilt
                for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
                    if line.startswith(i_list[i]):    # Schlagwortsuche
                        start_line = lines.index(line)    # Beginn relevanter Bereich
                    elif line.startswith(end_list[i]):    # Schlagwortsuche
                        end_line = lines.index(line)    # Ende relevanter Bereich
                Resulting_list = lines[start_line:end_line+1]    # relevanter Bereich wird in Liste gespeichert
                file.write(''.join(Resulting_list))    # Einfügen des relevanten Bereichs
        file.write('**')
if __name__ == "__main__":
    main()
    
    

# INPUT-FILE: WPS_MECH_Netz_Assembly
# FILENAME = "Job2D.inp"    # Das Programm und die Textdatei im gleichen Verzeichnis
index_list = ['*Heading','** PART INSTANCE: Blech-1','** PART INSTANCE: Elektrode_oben',
              '** PART INSTANCE: Blech-2','** PART INSTANCE: Elektrode_unten',
              '*Elset, elset=EALL','*Nset, nset=NALL,','*Elset, elset=SET-1',
              '*Elset, elset=SET-2','*Elset, elset=SET-3',
              '*Equation','*Nset, nset=BC_ELEKTRODE','*Nset, nset=NSET_TEMP',
              '*Nset, nset=NSET_E1','*Nset, nset=NSET_E2']    # Liste für Beginn des relevanten Bereiches

end_list = ['** ---','*Nset, nset=Blech-1','*Nset, nset=Elektrode_oben',
            '*Nset, nset=Blech-2','*Nset, nset=Elektrode_unten',
            '*Nset, nset=ELEKTRODE1','*Elset, elset=NALL,','*Nset, nset=SET-2',
            '*Nset, nset=SET-3','** Constraint: Eqn-1',
            '** MATERIALS','*Elset, elset=BC_ELEKTRODE','*Elset, elset=NSET_TEMP',
            '*Elset, elset=NSET_E1','*Elset, elset=NSET_E2']    # Liste für Ende des relevanten Bereiches

header_list = ['nset=LOAD_ELEKTRODE','nset=BC_TEMP','BC_Blechfix']    # Liste für Suchbegriffe
temp_list = []    # Liste zum Speichern von allen Inhalten
all_items = []    # Liste zum Speichern von relevanten Inhalten
header = ''    # Platzhalter

def main():
    with open('WPS_MECH_Netz_Assembly.txt', 'w') as file:    # Erstellung des Input-File
        for i in range(len(index_list)) and range(len(end_list)):    # Schleife welche über Elemente i der Listen iteriert
            with open(FILENAME, encoding="ascii") as input_file:    # Master-Input-File wird geöffnet
                lines = input_file.readlines()    # Dokument wird in einzelne Zeilen unterteilt
                for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
                    if line.startswith(index_list[i]):    # Schlagwortsuche
                        start_line = lines.index(line)    # Beginn relevanter Bereich
                    elif line.startswith(end_list[i]):    # Schlagwortsuche
                        end_line = lines.index(line)    # Ende relevanter Bereich
                Resulting_list = lines[start_line:end_line]    # relevanter Bereich wird in Liste gespeichert
                file.write(''.join(Resulting_list))    # Einfügen des relevanten Bereichs
                file.write('**')    # schreibt Inhalt inner der Schleife
                file.write('\n')    # schreibt Zeilenumbruch innerhalb der Schleife
                file.write('**')
                file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-1, MATERIAL=CuCrZr')    # schreibt Inhalt außerhalb der Schelife
        file.write('\n')    # schreibt Zeilenumbruch außerhalb der Schleife
        file.write('1.,')
        file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-2, MATERIAL=Al5182')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-3, MATERIAL=Al5182')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('**')
        file.write('\n')
# drei Begriff werden gesucht: nset=BC_TEMP, BC_Blechfix und nset=LOAD_ELEKTRODE (beinhaltet auch nset=LOAD_ELEKTRODE_REFERENZ)
        with open(FILENAME, encoding="ascii") as input_file:    # Master-Input-File wird geöffnet
            lines = input_file.readlines()    # Dokument wird in einzelne Zeilen unterteilt
            for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
                if line.startswith('*') and not(line.startswith('**')) :    # Bedingung für Beginn und Ende des relevanten Bereiches
                    if temp_list:    # überprüft ob Liste mit Inhalten gefüllt ist
                        appendlist = [header]    # Inhalte werden "header" zugewiesen
                        appendlist.append(list(temp_list))    # Inhalte werden in "temp_list" gelistet
                        
                        all_items.append(appendlist)    # Inhalte werden in Liste "all_items" übertragen
                        temp_list.clear()    # Inhalte der "temp_list" werden gelöscht
                        header = line    # Überschrift wird als Zeile definiert
                    else:    # Bedingung wenn "temp_list" keine Inhalte hat
                        header = line    # Überschrift wird als Zeile definiert
                else:    # wenn Bedingung für Beginn und Ende des relevanten Bereiches nicht zutrifft
                    temp_list.append(line)    # "temp_list" werden Inhalte der relevanten Zeilen (header) zugeordnet

        def search(header_list):
            for element in all_items:    # Schleife welche über die Elemente in der Liste "all_items" iteriert
                if header_list in element[0]:    # Bedingung falls Inhalt der "header_list" dem Element mit dem Index 0 entspricht
                    #print(element[0])
                    file.write(element[0])    # schreibt die Überschrift mit dem Index 0 in das Input-File
                    for x in element[1]:    # Schleife welche die Elemente mit dem Index 1 durchläuft
                        #print(x)
                        file.write(x)    # Element x wird in Liste geschrieben
                    file.write('**')
                    file.write('\n')
        
        search(header_list[0])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 0
        search(header_list[1])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 1
        search(header_list[2])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 2

if __name__ == "__main__":
    main()
    
# überflüssige Zeilen bzw. Bestandteile: Surf_load_Current



# INPUT-FILE: WPS_Elek_Elemente
# FILENAME = "Job2D.inp"    # Das Programm und die Textdatei im gleichen Verzeichnis
import os 
header_list = ['Element, type','Elset, elset=EALL','Elset, elset=SET',
               'Nset, nset=BC_ELEKTRODE','Nset, nset=LOAD_ELEKTRODE',
               'Nset, nset=LOAD_CURRENT','Nset, nset=BC_TEMP','Nset, nset=NSET_E1']    # Liste für Suchbegriffe
temp_list = []    # Liste zum Speichern von allen Inhalten
all_items = []    # Liste zum Speichern von relevanten Inhalten
header = ''

do_list = ['*Material, name=Al5182','*Electrical Conductivity','*Latent Heat','*Specific Heat']    # Liste für Beginn des relevanten Bereiches 
enddo_list = ['*Depvar','*Expansion','*Plastic','*User Defined Field']    # Liste für Ende des relevanten Bereiches

index_list = ['*Material, name=CuCrZr']    # Liste für Beginn des relevanten Bereiches
end_list = ['** INTERACTION PROPERTIES']    # Liste für Ende des relevanten Bereiches
 

def main():
    with open('WPS_Elek_Elemente_oE.txt', 'w') as file:    # Erstellung des Input-Files
        file.write('**')    # schreibt Inhalt
        file.write('\n')    # schreibt Zeilenumbruch
        file.write('*SOLID SECTION, ELSET=SET-1, MATERIAL=CuCrZr')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-2, MATERIAL=Al5182')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-3, MATERIAL=Al5182')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('**')
        file.write('\n')
        with open(FILENAME, encoding="ascii") as input_file:    # Master-input-File wird geöffnet           
            lines = input_file.readlines()    # Dokument wird in einzelne Zeilen unterteilt
            for line in lines:    # Schleife welche das Dokument Zeile für Zeile durchfläuft
                if line.startswith('*') and not(line.startswith('**')) :    # Bedingung für Beginn und Ende des relevanten Bereiches
                    if temp_list:    # überprüft ob Liste mit Inhalten gefüllt ist
                        appendlist = [header]    # Inhalte werden "header" zugewiesen
                        appendlist.append(list(temp_list))    # Inhalte werden in "temp_list" gelistet
                        
                        all_items.append(appendlist)    # Inhalte werden in Liste "all_items" übertragen
                        temp_list.clear()    # Inhalte der "temp_list" werden gelöscht
                        header = line    # Überschrift wird als Zeile definiert
                    else:    # Bedingung wenn "temp_list" keine Inhalte hat
                        header = line    # Überschirft wird als Linie definiert
                else:    # wenn Bedingung für Beginn und Ende des relevanten Bereiches nicht zutrifft
                    temp_list.append(line)    # "temp_list" werden Inhalte der relevanten Linien (header) zugeordnet

        def search(header_list):
            for element in all_items:    # Schleife welche über die Elemente in der Liste "all_items" iteriert
                if header_list in element[0]:    # Bedingung falls Inhalt der "header_list" dem Element mit dem Index 0 entspricht
                    #print(element[0])
                    file.write(element[0])    # schreibt die Überschrift mit dem Index 0 in das Input-File
                    for x in element[1]:    # Schleife welche die Elemente mit dem Index 1 durchläuft
                        #print(x)
                        file.write(x)    # Element x wird in Dokument geschrieben
                    file.write('**')    # schreibt Inhalt innerhalb der Schleife
                    file.write('\n')    # schreibt Zeilenumbruch innerhalb der Schleife
        
        search(header_list[0])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 0
        search(header_list[1])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 1
        search(header_list[2])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 2
        search(header_list[3])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 3
        search(header_list[4])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 4
        search(header_list[5])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 5
        search(header_list[6])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 6
        search(header_list[7])    # Suchbefehl für das Listenelement aus der Liste "header_list" mit dem Index 7
        
        with open(FILENAME, encoding="ascii") as input_file:    # Master-Input-File wird geöffnet
            lines = input_file.readlines()    # Dokument wird in einzelne Zeilen unterteilt
            for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
                if line.startswith('_SURF2_'):    # Schlagwortsuche (ohne Listenzugriff)
                    start_line = lines.index(line)    # Beginn relevanter Bereich
                elif line.startswith('** Constraint: Eqn-1'):    # Schlagwortsuche (ohne Listenzugriff)
                    end_line = lines.index(line)    # Ende relevanter Bereich
            Resulting_list = lines[start_line+1:end_line]    # relevanter Bereich wird in Liste gespeichert
            file.write(''.join(Resulting_list))    # Einfügen des relevanten Bereichs
            file.write('**')
            file.write('\n')

        file.write('**')    # schreibt Inhalt außerhalb der Schleife
        file.write('\n')    # schreibt Zeilenumbruch außerhalb der Schleife
        for i in range(len(do_list)) and range(len(enddo_list)):    # Schleife welche über Elemente i der Listen iteriert
            with open(FILENAME, encoding="ascii") as input_file:    # Master-Input-File wird geöffnet
                lines = input_file.readlines()    # Dokument wird in einzelne Zeilen unterteilt
                for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
                    if line.startswith(do_list[i]):    # Schlagwortsuche
                        start_line = lines.index(line)    # Beginn relevanter Bereich
                    elif line.startswith(enddo_list[i]):    # Schlagwortsuche
                        end_line = lines.index(line)    # Ende relevanter Bereich
                Resulting_list = lines[start_line:end_line]    # relevanter Bereich wird in Liste gespeichert              
                file.write(''.join(Resulting_list))    # Einfügen des relevanten Bereichs               
                
        for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
            if line.startswith(index_list[0]):    # Schlagwortsuche
                start_line1 = lines.index(line)    # Beginn relevanter Bereich
            elif line.startswith(end_list[0]):    # Schlagwortsuche
                end_line1 = lines.index(line)    # Ende relevanter Bereich
                    
        Resulting_list1 = lines[start_line1:end_line1-3]    # relevanter Bereich wird in Liste gespeichert
        file.write('**')
        file.write('\n')
        file.write(''.join(Resulting_list1))    # Einfügen des relevanten Bereichs 
        file.write('**')

if __name__ == "__main__":
    main()
    
# Änderung des Elementtyps für das Input-File WPS_Elek_Elemente.txt
# Löschen des Input-Files Elek_Elemente ohne die Abänderung der Elementtypen
def main():
    with open('WPS_Elek_Elemente_oE.txt') as lines, open('WPS_Elek_Elemente.txt', 'w') as output: # liest Input-File aus Zeile 249 ein und generiert neues Input-File
        for line in lines:    # Schleife welche das Master-Input-File Zeile für Zeile durchläuft
            if line == '*Element, type=CAX4\n':    # Bedingung für Übereinstimmung einer Zeile
                line = '*Element, type=DCAX4E\n'    # Anpassen der Zeile 
            if line == '*Element, type=CAX3\n':    # Bedingung für Übereinstimmung einer Zeile
                line = '*Element, type=DCAX3E\n'    # Anpassen der Zeile 
            output.write(line)    # Einfügen aller Inhalte des Input-File aus Zeile 249 mit den geänderten Zeilen
    datei= 'WPS_Elek_Elemente_oE.txt'    # Benennung des überflüssigen Input-Files
    if os.path.isfile(datei):    # falls Dabei existiert 
        os.remove(datei)    # Datei wird gelöscht
if __name__ == "__main__":
    main() 