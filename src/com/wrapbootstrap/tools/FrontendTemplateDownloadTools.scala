package com.wrapbootstrap.tools

import java.io._
import java.net.{URL}
import java.nio.file.{Files, Paths}

import scala.collection.mutable.HashSet
import scala.io.{Source}
import scala.util.matching.Regex.Match

/**
  * Created by admin on 2016/5/22.
  * html file in  different directory
  *
  */
class FrontendTemplateDownloadTools {

  val baseDir = "G:/Bootstrap/frontend/";
  val failureDir = "G:/Bootstrap/";

  def start(url: String) {
    println("--------------------------- " + url + " ---------------------------")
    val baseUrl = url.substring(0, url.lastIndexOf("/") + 1);
    val basePath = baseDir + baseUrl.replace("http://", "").replace("https://", "");
    val homeFile = baseDir + url.replace("http://", "").replace("https://", "");
    writeLog(url, 1);
    if (!Paths.get(basePath).toFile.exists()) {
      Files.createDirectories(Paths.get(basePath));
    }
    val writer = new PrintWriter(homeFile);
    val list = Source.fromURL(url, "UTF-8").getLines().toList;
    list.foreach(writer.write(_));
    writer.close();

    val htmlSet: HashSet[String] = new HashSet();
    val cjjSet: HashSet[String] = new HashSet();
    val fontSet: HashSet[String] = new HashSet();

    list.map(matchLine(_, htmlSet, 0, ""));
    htmlSet.filter((x: String) => x.endsWith(".html")).map(downLoadHtml(_, baseUrl, basePath, cjjSet));

    println("start download js file  -------------------------------------------------------------------------- js")
    cjjSet.filter((x: String) => x.endsWith(".js") && x.indexOf("http:") < 0 && x.indexOf("https:") < 0).map(downLoadJs(_, baseUrl, basePath));

    println("start download css file  -------------------------------------------------------------------------- cs")
    cjjSet.filter((x: String) => x.endsWith(".css") && x.indexOf("http:") < 0 && x.indexOf("https:") < 0).map(downLoadCss(_, baseUrl, basePath, fontSet))

    println("start download css internal font img file  -------------------------------------------------------------------------- css internal font img")
    val font = (x: String) => if (x.indexOf("?") > 0) x.substring(0, x.indexOf("?")) else if (x.indexOf("#") > 0) x.substring(0, x.indexOf("#")) else x
    fontSet.map(font).filter((x: String) => x.indexOf("data:") < 0 && !x.startsWith("#") && x.indexOf("http:") < 0 && x.indexOf("https:") < 0).map(downloadBinaryFile(_, baseUrl, basePath));

    println("start download img file -------------------------------------------------------------------------- img")
    cjjSet.filter((x: String) => (x.endsWith(".jpg") || x.endsWith(".png")) && x.indexOf("http:") < 0 && x.indexOf("https:") < 0).map(downloadBinaryFile(_, baseUrl, basePath));
    //cjjSet.foreach(println);
  }

  def matchLine(line: String, set: HashSet[String], flag: Int, dir: String) {
    val src = """src="([^"]+)"""".r;
    val href = """href="([^"]+)"""".r;
    val url ="""url\([^(]*\)""".r;
    if (flag == 2) {
      (url findAllMatchIn line).map(_ match { case Match(s) =>
        s.replace("url(", "").replace(")", "").replace("\"", "").replace("'", "")
      })
        .foreach(set add dir + _);
    } else {
      if (flag != 0) {
        (src findAllMatchIn line).map(_ match { case Match(s) =>
          s.replace("src=", "").replace("\"", "")
        }).foreach(set add dir + _);
      }
      (href findAllMatchIn line).map(_ match { case Match(s) =>
        s.replace("href=", "").replace("\"", "")
      }).foreach(set add dir + _);
    }

  }

  def downLoadHtml(fileName: String, baseUrl: String, basePath: String, set: HashSet[String]): Unit = {
    val url = baseUrl + fileName;
    val htmlFile = basePath + fileName;
    val htmlDir = fileName.substring(0, fileName.lastIndexOf("/") + 1);
    if (!Paths.get(htmlFile).getParent.toFile.exists()) {
      Files.createDirectories(Paths.get(htmlFile).getParent);
    }
    val writer = new PrintWriter(htmlFile);
    val bw: BufferedWriter = new BufferedWriter(writer);
    try {
      val list = Source.fromURL(url, "UTF-8").getLines().toList;
      list.foreach((x: String) => {
        bw.write(x); bw.newLine();
      });
      bw.close();
      println("suc:" + fileName);
      list.map(matchLine(_, set, 1, htmlDir));
    } catch {
      case ex: java.lang.Exception => println("html file download failure:" + url); writeLog(url, 0);
    }

  }

  def downLoadJs(fileName: String, baseUrl: String, basePath: String): Unit = {
    val url = baseUrl + fileName;
    val jsFile = basePath + fileName;
    if (!Paths.get(jsFile).getParent.toFile.exists()) {
      Files.createDirectories(Paths.get(jsFile).getParent);
    }
    val writer = new PrintWriter(jsFile);
    val bw: BufferedWriter = new BufferedWriter(writer);
    try {
      val list = Source.fromURL(url, "UTF-8").getLines().toList;
      list.foreach((x: String) => {
        bw.write(x); bw.newLine()
      });
      bw.close();
      println("suc :" + fileName);
    } catch {
      case ex: java.lang.Exception => println("js file download failure:" + url); writeLog(url, 0);
    }

  }


  def downLoadCss(fileName: String, baseUrl: String, basePath: String, set: HashSet[String]): Unit = {
    val url = baseUrl + fileName;
    val cssFile = basePath + fileName;
    val cssDir = fileName.substring(0, fileName.lastIndexOf("/") + 1);
    if (!Paths.get(cssFile).getParent.toFile.exists()) {
      Files.createDirectories(Paths.get(cssFile).getParent);
    }
    try {
      val writer = new PrintWriter(cssFile);
      val bw: BufferedWriter = new BufferedWriter(writer);
      val list = Source.fromURL(url, "UTF-8").getLines().toList;
      list.foreach((x: String) => {
        bw.write(x); bw.newLine()
      });
      bw.close();
      println("css file suc :" + fileName);
      list.map(matchLine(_, set, 2, cssDir));
    } catch {
      case ex: java.lang.Exception => println(" css file download failure:" + url); writeLog(url, 0);
    }

  }


  def downloadBinaryFile(fileName: String, baseUrl: String, basePath: String): Unit = {
    val url = baseUrl + fileName;
    val imgFile = basePath + fileName;
    if (!Paths.get(imgFile).getParent.toFile.exists()) {
      Files.createDirectories(Paths.get(imgFile).getParent);
    }
    val u: URL = new URL(url);
    try {
      val dataInputStream: DataInputStream = new DataInputStream(u.openStream());
      val fileOutputStream: FileOutputStream = new FileOutputStream(new File(imgFile));
      val buffer: Array[Byte] = new Array[Byte](1024);
      var length: Int = 0;
      while ( {
        length = dataInputStream.read(buffer);
        length > 0
      }) {
        fileOutputStream.write(buffer, 0, length);
      }
      dataInputStream.close();
      fileOutputStream.close();
      println("binary file suc :" + fileName);
    } catch {
      case ex: java.lang.Exception => println("failure:" + url); writeLog(url, 0);
    }
  }

  def writeLog(log: String, flag: Int): Unit = {
    val writer: FileWriter = new FileWriter(failureDir + "failure.txt", true);
    val bw: BufferedWriter = new BufferedWriter(writer);
    bw.newLine();
    if (flag == 0) {
      bw.write("        --" + log);
    } else {
      bw.write(log);
    }
    bw.close();
  }


}

object FrontendTest {
  def main(args: Array[String]) {
    var list = List[String](); //40

//    list+:="http://htmlstream.com/preview/unify-v1.9.4/index.html"
//    list +:= "http://htmlcoder.me/preview/the_project/v.1.2/index.html"//首页图片有问题
    list +:= "http://razonartificial.com/themes/reason/v1.4.7/index.html"
    list +:= "http://preview.webpixels.ro/boomerang-v2.0.2/index.html"
    list +:= "http://theme.stepofweb.com/Atropos/v1.7/HTML/index.html"
    list +:= "http://htmlcoder.me/preview/idea/v.1.4/html/index.html"


    list.map(new FrontendTemplateDownloadTools().start(_))


  }
}
