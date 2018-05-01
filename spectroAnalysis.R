# Define function to plot spectrogram (credits: Don's version of an I400 demo, rev. April 2008. (Indiana University)

spectrogram <- function(y, N) {
  nFrames <- floor(length(y)/N)
  spect <- matrix(0,nFrames,N/2)
  v <- seq(from=0,by=2*pi/N,length=N)      # N evenly spaced pts, 0 -- 2*pi
  win <- (1 + cos(v-pi))/2                 # use a cosine window	
  for (t in 1:nFrames) {
    chunk  <- y[(1+(t-1)*N):(t*N)]
    Y <- fft(chunk*win)
    spect[t,] <- Mod(Y[1:(N/2)])
  }
  
  # Show energy levels in greyscale
  bright <- seq(0,1,by=.01)
  power <- .2
  bright <- seq(0,1,by=.01)^power
  grey <- rgb(bright,bright,bright)
  
  image(spect,col=grey)
}


# FUnction trim to trim spaces present in metadata columns
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# Read metadata file that contains filenames and genres
metaData = read.csv("metadata_2013.csv")

# Trim spaces from Genre and filename columns
metaData$Genre = trim(metaData$Genre)
metaData$file_name = trim(metaData$file_name)

# Get unique list of Genres.
genreList = unique(metaData$Genre)
freq <- 22050

# For each genre, take first 10 files and for each file, extract the song portion from 10-15 seconds 
# paste all these excerpts and assign them to one file per genre 
for (i in 1:length(genreList))
{
  meta = metaData[metaData$Genre == genreList[i],]
  meta = meta[1:10,]
  
  fileNames = meta$file_name
  dummySong = readMP3("2.mp3")
  dummySong = cutw(dummySong, from=0, to=0, output="Wave") 
  
  for (j in 1:length(fileNames))
  {
    song = readMP3(trim(as.character(fileNames[j])))
    song = cutw(song,from=10, to=15, output="Wave") 
    dummySong = pastew(dummySong,song,f = freq)
  }
  assign(genreList[i],dummySong)
}


plot(c(1,2,3),c(1,2,3))

# Plot spectrogram for Blues genre 
spectrogram(Wave(Blues)@left,1024)

# Plot spectrogram for Pop genre 
spectrogram(Wave(Pop)@left,1024)

# Plot spectrogram for Rock genre 
spectrogram(Wave(Rock)@left,1024)

# Plot spectrogram for Classical genre 
spectrogram(Wave(Classical)@left,1024)


spectrogram(Wave(Electronic)@left,1024)