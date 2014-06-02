package utils.compression

import java.util.zip.{ZipOutputStream, ZipEntry}
import java.io.ByteArrayOutputStream

/**
 * Created by smcho on 5/31/14.
 */
object Zip {
  // http://stackoverflow.com/questions/357851/in-java-how-to-zip-file-from-byte-array
  def zipBytes(input: Array[Byte]) {
    val baos = new ByteArrayOutputStream();
    val zos = new ZipOutputStream(baos);
    //val entry = new ZipEntry(filename);
    //entry.setSize(input.length);
    //zos.putNextEntry(entry);
    zos.write(input);
    zos.closeEntry();
    zos.close();
    return baos.toByteArray();
  }
}
