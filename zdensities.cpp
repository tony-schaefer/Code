#include <iostream> //i need these for some reason
#include <fstream>
#include <sstream>
#include <stdlib.h>
#include <cmath>
#include <iomanip>
#include "gromacs/fileio/xtcio.h" //not sure if it can find this or if this is working

std::string trim(std::string str) { //function to trim whitespace (like str.strip() in python)
    size_t first = str.find_first_not_of(' ');
    size_t last = str.find_last_not_of(' ');
    return str.substr(first,(last-first+1));
}

int main(int argc, char* argv[]) {
    int n,bins=100,ax=2,itime=0,ftime=-1,clock=0;
    std::string infile,framefile,axis,format,xtcfile="some.xtc";
    int natoms;
    std::string str,head,buffer[3],residues[100];

    if( argc < 2 ) {
        std::cout << "zdensities is the c++ version of densiness" << std::endl;
        std::cout << "-f    : input gro trajectory file" << std::endl;
        std::cout << "-bins : number of bins" << std::endl;
        std::cout << "-axis : calculate density as a function of position on the x, y, or z axis" << std::endl;
        std::cout << "-itime: time to start calculating density" << std::endl;
        std::cout << "-itime: time to stop calculating density" << std::endl;
        exit(0);
    }

    for(n=0; n < argc; n+=1){
        if(std::string(argv[n]) == "-f"){
            infile = argv[n+1];
            if(infile.substr(infile.length()-3) == "gro" ){
                format = "gro";
            } else if(infile.substr(infile.length()-3) == "xtc"){
                format = "xtc";
            }
        }
        if(std::string(argv[n]) == "-s"){
            framefile = argv[n+1];
        }
        if(std::string(argv[n]) == "-bins"){
            bins = strtol(argv[n+1],NULL,10);
        }
        if(std::string(argv[n]) == "-axis"){
            axis = std::string(argv[n+1]);
            if( axis == "z" || axis == "2" ){
                ax=2;
            } else if( axis == "y" || axis == "1"){
                ax=1;
            } else if( axis == "x" || axis == "0"){
                ax=0;
            }
        }
        if(std::string(argv[n]) == "-ftime"){
            ftime = strtol(argv[n+1],NULL,10);
        }
        if(std::string(argv[n]) == "-itime"){
            itime = strtol(argv[n+1],NULL,10);
        }
    }
    if( format == "xtc"){
        xtcfile=infile;
        infile=framefile;
    }
    
    std::ifstream thefile (infile.c_str());
    t_fileio* compfile (char(xtcfile));
    getline(thefile,head);
    thefile >> natoms; //read number of atoms
    getline(thefile,str); //advance line
    std::string molres[natoms],atres[natoms];
    int molnum[natoms],atnum[natoms],i,j,k,l,res=0; //initialize variables 
    float time,prec;
    long double box[3][3],xyz[natoms][3],xtcxyz[3*natoms],amass[natoms],bin[bins],binmass[bins][100],vol,dz;
    
    if(format == "xtc"){
        for(i=0; i < natoms ; i+=1){ //read atom and molecule info
            getline(thefile,str); //there has to be an easier way to read formatted input
            molnum[i] = strtol(str.substr(0,5).c_str(),NULL,10) ;
            molres[i] = trim(str.substr(5,4)) ;
            atres[i] = trim(str.substr(11,4)) ;
            atnum[i] = strtol(str.substr(15,5).c_str(),NULL,10) ;
            if(k == 1){
                for(l=0; l <= res; l+=1){ //find residues
                    if(molres[i] == residues[l]){
                        break;
                    } else if(l == res){
                        residues[res]=molres[i];
                        std::cout << "Found " << molres[i] << " residue" << std::endl;
                        res=res+1;
                        break;
                    }
                }
            }

            if(atres[i].substr(0,1) == "H"){ //set masses
                amass[i]=1.008;
            } else if(atres[i].substr(0,1) == "O"){
                amass[i]=15.9994;
            } else if(atres[i].substr(0,1) == "C"){
                amass[i]=12.011;
            } else if(atres[i].substr(0,1) == "N"){
                amass[i]=14.0067;
            }
            if(atres[i].substr(0,3) == "CL "){
                amass[i]=35.453;
            } else if(atres[i].substr(0,3) == "NA "){
                amass[i]=22.98977;
                std::cout << amass[i] << std::endl;
            }
        }
    }
    

    k=0;
    bin[0]=0.0;
    for(l=0; l < 100; l+=1){
        binmass[0][l]=0.0;
    }

    do {
        
        k=k+1;
        if( k > 1 ){
            if( format == "gro" ){
                getline(thefile,head);
                if( thefile.eof() ){
                    break;
                }
            }
            if( k > ftime && ftime > 0){
                break;
            }
            if( format == "gro" ){
                getline(thefile,str);
            }
        }
        if( k % 100 == 0 ){
            std::cout << k << std::endl;
        }

        if( format == "gro" ){
            for(i=0; i < natoms ; i+=1){ //read atom and molecule info
                getline(thefile,str); //there has to be an easier way to read formatted input
                molnum[i] = strtol(str.substr(0,5).c_str(),NULL,10) ;
                molres[i] = trim(str.substr(5,4)) ;
                atres[i] = trim(str.substr(11,4)) ;
                atnum[i] = strtol(str.substr(15,5).c_str(),NULL,10) ;
                xyz[i][0] = strtof(str.substr(22,6).c_str(),NULL);
                xyz[i][1] = strtof(str.substr(30,6).c_str(),NULL);
                xyz[i][2] = strtof(str.substr(38,6).c_str(),NULL);
                if(k == 1){
                    for(l=0; l <= res; l+=1){ //find residues
                        if(molres[i] == residues[l]){
                            break;
                        } else if(l == res){
                            residues[res]=molres[i];
                            std::cout << "Found " << molres[i] << " residue" << std::endl;
                            res=res+1;
                            break;
                        }
                    }

                    if(atres[i].substr(0,1) == "H"){ //set masses
                        amass[i]=1.008;
                    } else if(atres[i].substr(0,1) == "O"){
                        amass[i]=15.9994;
                    } else if(atres[i].substr(0,1) == "C"){
                        amass[i]=12.011;
                    } else if(atres[i].substr(0,1) == "N"){
                        amass[i]=14.0067;
                    }
                    if(atres[i].substr(0,3) == "CL "){
                        amass[i]=35.453;
                    } else if(atres[i].substr(0,3) == "NA "){
                        amass[i]=22.98977;
                    }   
                }
            } 
        
            getline(thefile,str);
            std::stringstream buff(str);
            for(i=0; i < 3; i+=1){ //read box dimensions 
                buff >> buffer[i] ;
                box[i][i]=strtof(buffer[i].c_str(),NULL);
            }
        } else if ( format == "xtc" ){
            //read_next_xtc (compfile, natoms, k, time, box, xtcxyz, prec);
            std::cout << "reading xtc is not implemented" << std:: endl;
            break;
        }

        if(k >= itime){        

            clock = clock + 1;

            dz=box[ax][ax]/double(bins); //how long is each bin
            if(ax == 2){
                vol=dz*box[0][0]*box[1][1]; // what is the volume of each bin
            } else if(ax == 1){
                vol=dz*box[0][0]*box[2][2];
            } else if(ax == 0){
                vol=dz*box[1][1]*box[2][2];
            }

            for(j=1; j <= bins; j+=1){ //set bin limits
                bin[j]=bin[j-1]+dz;
                if(k == 1){
                    for (l=0; l < res; l+=1){
                        binmass[j][l]=0.0;
                    }
                }
            }
    
            for(j=0; j < bins; j+=1){ //find mass in each bin
                for(i=0; i < natoms; i+=1){
                    if(xyz[i][ax] > box[ax][ax]){ // move things for pbc
                        xyz[i][ax]=xyz[i][ax]-box[ax][ax];
                    } else if(xyz[i][ax] < 0.0){
                        xyz[i][ax]=box[ax][ax]-xyz[i][ax];
                    }
                    for(l=0; l < res; l+=1){
                        if(xyz[i][ax] >= bin[j] and xyz[i][ax] < bin[j+1] and molres[i] == residues[l]){
                            binmass[j][l]=binmass[j][l]+amass[i];
                        }
                    }
                }
            }
        }

    } while( ! thefile.eof() );

    std::ofstream outfile ("density.out"); //open out file
    for(j=0; j < bins; j+=1){
        outfile << std::setprecision(10) << bin[j]+dz/2. << " ";
        for(l=0; l < res; l+=1){
            binmass[j][l]=binmass[j][l]/(vol*0.602214*double(clock)); //find density in g/L, .gro files use nm for distance
            outfile << std::setprecision(10) << binmass[j][l] << " "; //write histograms and density in each to file
        }
        outfile << std::endl;
    }

    outfile.close();
    
    thefile.close(); //close file
    
    return 0; //exit successfully

}
