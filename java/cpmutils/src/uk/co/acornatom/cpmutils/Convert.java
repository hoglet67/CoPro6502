package uk.co.acornatom.cpmutils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Convert {
	
	int sectorSize = 128;
	int sectorsPerTrack = 20;

	
	// Acorn CPM disks are 80 Track Double Sided (400K)
	int acornDiskSize = sectorSize * sectorsPerTrack * 80 * 2;

	// Acorn CPM disks have a directory starting at track 3
	int acornDiskOffset = sectorSize * sectorsPerTrack * 3;
	
	int[] physSectorMap = new int[] {
		    0, 1, 2, 3,
		    8, 9, 10, 11,
		    16, 17, 18, 19,
		    4, 5, 6, 7,
		    12, 13, 14, 15
		};
	
	public Convert() {
	}
	
	private byte[] readImage(File file) throws IOException {
		Path path = Paths.get(file.getAbsolutePath());
		return Files.readAllBytes(path);
	}

	private void writeFile(File file, byte[] bytes) throws IOException {
		FileOutputStream fos = new FileOutputStream(file);
		int offset = 0;
		fos.write(bytes, offset, bytes.length - offset);
		fos.close();
	}

	private void convertAcornToStandard(File srcFile, File dstFile) throws IOException {

		byte[] src = readImage(srcFile);
		
		int numBytes = src.length;
		int numSectors = numBytes / sectorSize;
		int numTracks = numSectors / sectorsPerTrack;
		
		System.out.println("  numBytes = " + numBytes);
		System.out.println("numSectors = " + numSectors);
		System.out.println(" numTracks = " + numTracks);
		
		byte[] dst = new byte[src.length];
		for (int i = 0; i < dst.length; i++) {
			dst[i] = (byte) 0xaa;
		}
		    
		for (int logTrack = 0; logTrack < numTracks; logTrack++) {
			for (int logSector = 0; logSector < sectorsPerTrack; logSector++) {
			
				// Logical tracks got 0..159
				// Physical tracks 0 159 1 158 2 157 .. 79 80
				// 0->0
				// 1->2
				// 2->4
				// ..
				// 79->158
				// 80->159
				// 81->157
				// ..
				// 159->1
				
				int phyTrack = (logTrack < 80) ? (logTrack * 2) : (319 - logTrack * 2); 
			
				// Logical CP/M   Logical disk   Physical disk
				// record         sector         sector
				// (128 bytes)    (512 byte)     (256 bytes)
				// 0,1,2,3        0,0,0,0        0,0,1,1
				// 4,5,6,7        1,1,1,1        4,4,5,5
				// 8,9,10,11      2,2,2,2        8,8,9,9
				// 12,13,14,15    3,3,3,3        2,2,3,3
				// 16,17,18,19    4,4,4,4        6,6,7,7
				
				int phySector = physSectorMap[logSector];
				
				int srcOffset = (phyTrack * sectorsPerTrack + phySector) * sectorSize;
				int dstOffset = (logTrack * sectorsPerTrack + logSector) * sectorSize;
				for (int i = 0; i < sectorSize; i++) {
					dst[dstOffset + i] = src[srcOffset + i];
				}
			}
		}
		
		writeFile(dstFile, dst);
	}
	
	private void convertStandardToAcorn(File srcFile, File dstFile) throws IOException {

		byte[] src = readImage(srcFile);
		
		int numBytes = src.length;
		int numSectors = numBytes / sectorSize;
		int numTracks = numSectors / sectorsPerTrack;
		
		byte[] dst = new byte[acornDiskSize];

		for (int i = 0; i < dst.length; i++) {
			dst[i] = (byte) 0xaa;
		}
		    
		for (int logTrack = 0; logTrack < numTracks; logTrack++) {
			for (int logSector = 0; logSector < sectorsPerTrack; logSector++) {
			
				// Logical tracks got 0..159
				// Physical tracks 0 159 1 158 2 157 .. 79 80
				// 0->0
				// 1->2
				// 2->4
				// ..
				// 79->158
				// 80->159
				// 81->157
				// ..
				// 159->1
				
				int phyTrack = (logTrack < 80) ? (logTrack * 2) : (319 - logTrack * 2); 
			
				// Logical CP/M   Logical disk   Physical disk
				// record         sector         sector
				// (128 bytes)    (512 byte)     (256 bytes)
				// 0,1,2,3        0,0,0,0        0,0,1,1
				// 4,5,6,7        1,1,1,1        4,4,5,5
				// 8,9,10,11      2,2,2,2        8,8,9,9
				// 12,13,14,15    3,3,3,3        2,2,3,3
				// 16,17,18,19    4,4,4,4        6,6,7,7
				
				int phySector = physSectorMap[logSector];
				
				int srcOffset = (logTrack * sectorsPerTrack + logSector) * sectorSize;
				int dstOffset = (phyTrack * sectorsPerTrack + phySector) * sectorSize;
				for (int i = 0; i < sectorSize; i++) {
					dst[dstOffset + i] = src[srcOffset + i];
				}
			}
		}
		
		writeFile(dstFile, dst);
	}
	
	

	public static final void main(String[] args) {
		try {
			if (args.length != 3) {
				System.err.println("usage: java -jar cpmutils.jar <A2S|S2A> <Src CPM Image> <Dst CPM Image>");
				System.exit(1);
			}
			String mode = args[0].toLowerCase();

			File srcFile = new File(args[1]);
			if (!srcFile.exists()) {
				System.err.println("CPM Image: " + srcFile + " does not exist");
				System.exit(1);
			}
			if (!srcFile.isFile()) {
				System.err.println("CPM Image: " + srcFile + " is not a file");
				System.exit(1);
			}
			File dstFile = new File(args[2]);
			

			Convert c = new Convert();

			if (mode.equals("a2s")) {
				c.convertAcornToStandard(srcFile, dstFile);
			} else if (mode.equals("s2a")) {
				c.convertStandardToAcorn(srcFile, dstFile);				
			} else {
				System.err.println("Unknown mode: " + mode);
				System.exit(1);
			}

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
