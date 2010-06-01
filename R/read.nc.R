#this function was modified from retrieve.nc of the clim.pact package version 2.2-41.
#reason for copying the function is that it has changed over time and functions of the
#crrent package require 'no changes' to the output data structure.

read.nc <- function (filename = file.path("data", "air.mon.mean.nc"), v.nam = "AUTO", 
    l.scale = FALSE, greenwich = TRUE, silent = FALSE, x.nam = "lon", 
    y.nam = "lat", z.nam = "lev", t.nam = "tim", x.rng = NULL, 
    y.rng = NULL, z.rng = NULL, t.rng = NULL, force.chron = TRUE, 
    force365.25 = FALSE, regular = TRUE, daysayear = 365.25, 
    forceBC = TRUE, use.cdfcont = FALSE, torg = NULL, t.unit = NULL, 
    miss2na = TRUE) 
{
	#some functions needed for this function
	mod <- 	function (x, y) 
	{
		x1 <- trunc(trunc(x/y) * y)
		z <- trunc(x) - x1
		z
	}
	
	fixField <- function (x, torg = NULL, t.unit = NULL, scal = NULL, offs = NULL, 
    x.rng = NULL, y.rng = NULL, z.rng = NULL, t.rng = NULL, greenwich = TRUE) 
	{
		tim <- x$tim
		lon <- x$lon
		lat <- x$lat
		mm <- x$mm
		yy <- x$yy
		dd <- x$dd
		if (!is.null(torg)) {
			dsh <- instring("-", torg)
			print(dsh)
			yy0 <- as.numeric(substr(torg, dsh[2] + 1, dsh[2] + 4))
			dd0 <- as.numeric(substr(torg, 1, dsh[1] - 1))
			mm0 <- switch(tolower(substr(torg, dsh[1] + 1, dsh[2] - 
				1)), jan = 1, feb = 2, mar = 3, apr = 4, may = 5, 
				jun = 6, jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, 
				dec = 12)
			print(paste("Time origin: (year-month-day)", yy0, "-", 
				mm0, "-", dd0))
			if (yy0[1] == 0) {
				print("There is no year zero (Press et al., Numerical recipies)")
				print("'> print(julday(1,1,1)-julday(1,1,-1))' gives 365")
				print("julday wont work unless the time is fixed")
				print("year0 is set to 1, and 365 days is subtracted from tim")
				if (substr(tolower(t.unit), 1, 4) == "hour") 
					tim <- tim - 365 * 24
				if (substr(tolower(t.unit), 1, 3) == "day") 
					tim <- tim - 365
				if (substr(tolower(t.unit), 1, 3) == "mon") 
					tim <- tim - 12
				if (substr(tolower(t.unit), 1, 5) == "year") 
					tim <- tim - 1
				yy0 <- 1
			}
			if (is.null(t.unit)) 
				t.unit <- x$dat.att$t.unit
			print(paste("Time unit:", tolower(t.unit)))
			if (substr(tolower(t.unit), 1, 3) == "mon") {
				tim <- floor(tim)
				mm <- mod(mm0 + tim - 1, 12) + 1
				yy <- yy0 + floor((tim + mm0 - 1)/12)
				dd <- rep(15, length(tim))
				obj.type <- "monthly.field.object"
			}
			else if (substr(tolower(t.unit), 1, 3) == "day") {
				mmddyy <- caldat(tim + julday(mm0, dd0, yy0))
				mm <- mmddyy$month
				yy <- mmddyy$year
				dd <- mmddyy$day
				obj.type <- "daily.field.object"
			}
			else if (substr(tolower(t.unit), 1, 4) == "hour") {
				mmddyy <- caldat(tim/24 + julday(mm0, dd0, yy0))
				mm <- mmddyy$month
				yy <- mmddyy$year
				dd <- mmddyy$day
				t.unit <- "day"
				obj.type <- "field.object"
			}
		}
		else torg <- x$dat.att$torg
		if (!is.null(scal)) 
			x$dat <- x$dat + scal
		if (!is.null(offs)) 
			x$dat <- x$dat + offs
		if (greenwich) {
			lon[lon > 180] <- lon[lon > 180] - 360
		}
		if (!is.null(x.rng)) {
			print(range(lon))
			print("Extract longitudes:")
			print(x.rng)
			x.keep <- (lon >= min(x.rng)) & (lon <= max(x.rng))
			if (nd == 3) 
				dat <- dat[, , x.keep]
			else dat <- dat[, , , x.keep]
			lon <- lon[x.keep]
			id.lon <- id.lon[x.keep]
			id.x <- id.x[, x.keep]
			x.srt <- order(lon)
			lon <- lon[x.srt]
			if (nd == 3) 
				dat <- dat[, , x.srt]
			else dat <- dat[, , , x.srt]
		}
		if (!is.null(y.rng)) {
			print(range(lat))
			print("Extract latitudes:")
			print(y.rng)
			y.keep <- (lat >= min(y.rng)) & (lat <= max(y.rng))
			if (nd == 3) 
				dat <- dat[, y.keep, ]
			else dat <- dat[, , y.keep, ]
			lat <- lat[y.keep]
			id.lat <- id.lat[y.keep]
			id.x <- id.x[y.keep, ]
			y.srt <- order(lat)
			lat <- lat[y.srt]
			if (nd == 3) 
				dat <- dat[, y.srt]
			else dat <- dat[, , y.srt]
		}
		if (!is.null(t.rng)) {
			print(range(tim))
			print("Extract tims:")
			print(tim)
			t.keep <- (tim >= min(t.rng)) & (tim <= max(t.rng))
			if (nd == 3) 
				dat <- dat[t.keep, , ]
			else dat <- dat[t.keep, , , ]
			tim <- tim[t.keep]
			id.t <- id.t[t.keep]
			yy <- yy[t.keep]
			mm <- mm[t.keep]
			dd <- dd[t.keep]
		}
		x$tim <- tim
		x$lon <- lon
		x$lat <- lat
		x$yy <- yy
		x$mm <- mm
		x$dd <- dd
		x$dat.att$t.unit <- t.unit
		x$dat.att$torg <- torg
		x$dat.att$scale.factor <- scal
		x$dat.att$add.offset <- offs
		if (is.null(x$dat.att$fixes)) 
			x$dat.att$fixes <- paste("fixField ", date(), ": ", torg, 
				t.unit, scal, offs, sep = "")
		else x$dat.att$fixes <- paste(x$dat.att$fixes, date(), ": ", 
			torg, t.unit, scal, offs, sep = "")
		invisible(x)
	}

	caldat <- function (julian) 
	{
		igreg = 2299161
		julian <- trunc(julian)
		jalpha <- julian * 0
		ja <- julian * 0
		im <- (julian >= igreg)
		if (sum(im) > 0) {
			jalpha[im] <- trunc(((julian - 1867216) - 0.25)/36524.25)
			ja[im] <- julian + 1 + jalpha - trunc(0.25 * jalpha)
		}
		im <- (julian < igreg)
		if (sum(im) > 0) 
			ja[im] <- julian[im]
		jb <- ja + 1524
		jc <- trunc(6680 + ((jb - 2439870) - 122.1)/365.25)
		jd <- 365 * jc + trunc(0.25 * jc)
		je <- trunc((jb - jd)/30.6001)
		id <- jb - jd - trunc(30.6001 * je)
		mm <- je - 1
		im <- (mm > 12)
		if (sum(im) > 0) 
			mm[im] <- mm[im] - 12
		iyyy <- jc - 4715
		im <- (mm > 2)
		if (sum(im) > 0) 
			iyyy[im] <- iyyy[im] - 1
		im <- (iyyy <= 0)
		if (sum(im) > 0) 
			iyyy <- iyyy - 1
		caldat <- list(month = mm, day = id, year = iyyy)
		invisible(caldat)
	}

	julday <- function (mm, id, iyyy) 
	{
		igreg <- 588829
		mm <- trunc(mm)
		id <- trunc(id)
		iyyy <- trunc(iyyy)
		im <- (iyyy == 0)
		if (sum(im, na.rm = TRUE) > 0) 
			return("There is no year zero!")
		if ((length(mm) != length(id)) | (length(mm) != length(iyyy)) | 
			(length(iyyy) != length(id))) 
			return("The vectors must have same length!")
		im <- (iyyy < 0)
		if (sum(im) > 0) 
			iyyy[im] <- iyyy[im] + 1
		jy <- mm * 0
		jm <- mm * 0
		ja <- mm * 0
		im <- (mm > 2)
		if (sum(im) > 0) {
			jy[im] <- iyyy[im]
			jm[im] <- mm[im] + 1
		}
		im <- (mm <= 2)
		if (sum(im) > 0) {
			jy[im] <- iyyy[im] - 1
			jm[im] <- mm[im] + 13
		}
		jul <- trunc(365.25 * jy) + trunc(30.6001 * jm) + id + 1720995
		im <- (id + 31 * (mm + 12 * iyyy) >= igreg)
		if (sum(im) > 0) {
			ja[im] <- trunc(0.01 * jy)
			jul[im] <- jul + 2 - ja[im] + trunc(0.25 * ja[im])
		}
		julday <- jul
		invisible(julday)
	}

	datestr2num <- function (datestr, vec = TRUE)
	{
		dsh <- instring("-", datestr)
		spc <- instring(" ", datestr)
		if (dsh[1] == 0) {
			dot <- instring(".", datestr)
			com <- instring(",", datestr)
			sls <- instring("/", datestr)
			if (length(dot) == 2)
				dsh <- dot
			else if (length(com) == 2)
				dsh <- com
			else if (length(sls) == 2)
				dsh <- sls
			else if (length(spc) >= 2) {
				dsh <- spc[1:2]
				spc <- 0
			}
		}
		if (spc == 0)
			spc <- nchar(datestr)
		if (dsh[1] == 3 & dsh[2] == 7) {
			yy0 <- as.numeric(substr(datestr, 8, 11))
			mm0 <- switch(tolower(substr(datestr, 4, 6)), jan = 1,
				feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7,
				aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)
			dd0 <- as.numeric(substr(datestr, 1, 2))
		}
		if (dsh[1] == 2 & dsh[2] == 6) {
			yy0 <- as.numeric(substr(datestr, 7, 11))
			mm0 <- switch(tolower(substr(datestr, 3, 5)), jan = 1,
				feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7,
				aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)
			dd0 <- as.numeric(substr(datestr, 1, 1))
		}
		if (dsh[1] == 5 & dsh[2] == 9) {
			yy0 <- as.numeric(substr(datestr, 1, 4))
			mm0 <- switch(tolower(substr(datestr, 6, 8)), jan = 1,
				feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7,
				aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)
			dd0 <- as.numeric(substr(datestr, 10, 11))
		}
		if (dsh[1] == 5 & dsh[2] == 8) {
			yy0 <- as.numeric(substr(datestr, 1, 4))
			mm0 <- as.numeric(substr(datestr, 6, 7))
			dd0 <- as.numeric(substr(datestr, 9, 10))
			if (mm0 > 12) {
				a <- mm0
				mm0 <- mm0
				dd0 <- a
				rm(a)
				gc(reset = TRUE)
			}
		}
		if (dsh[1] == 3 & dsh[2] == 6) {
			yy0 <- as.numeric(substr(datestr, 7, 10))
			mm0 <- as.numeric(substr(datestr, 4, 5))
			dd0 <- as.numeric(substr(datestr, 1, 2))
		}
		if (dsh[1] == 5 & dsh[2] == 7) {
			yy0 <- as.numeric(substr(datestr, 1, 4))
			mm0 <- as.numeric(substr(datestr, 6, 6))
			dd0 <- as.numeric(substr(datestr, 8, 9))
		}
		if (dsh[1] == 2 & dsh[2] == 4) {
			dd0 <- as.numeric(substr(datestr, 1, 1))
			mm0 <- as.numeric(substr(datestr, 3, 3))
			yy0 <- as.numeric(substr(datestr, 5, spc[1]))
		}
		if (dsh[1] == 0) {
			dd0 <- 1
			mm0 <- 1
			yy0 <- as.numeric(datestr)
		}
		if (vec)
			datestr2num <- c(yy0, mm0, dd0)
		else datestr2num <- yy0 + mm0/12 + dd0/31
		datestr2num
	}

	instring <- function (c, target, case.match = TRUE) 
	{
		l <- nchar(target)
		if (!case.match) {
			c <- lower.case(c)
			target <- lower.case(target)
		}
		nc <- nchar(c)
		if (nc == 1) {
			pos <- 0
			for (i in 1:l) {
				tst <- substr(target, i, i)
				if (tst == c) 
					pos <- c(pos, i)
			}
			if (length(pos) > 1) 
				pos <- pos[-1]
		}
		else {
			spos <- rep(NA, nc * l)
			dim(spos) <- c(nc, l)
			for (j in 1:nc) {
				a <- instring(substr(c, j, j), target)
				if (length(a) > 0) {
					if (j > 1) {
					  a.match <- is.element(a, spos[j - 1, ] + 1)
					  a <- a[a.match]
					  p.match <- is.element(spos[j - 1, ], a - 1)
					  spos <- spos[, p.match]
					  dim(spos) <- c(nc, sum(p.match))
					}
					if (length(a) < dim(spos)[2]) {
					  spos <- spos[, 1:length(a)]
					  dim(spos) <- c(nc, length(a))
					}
					spos[j, ] <- a
				}
				else spos[j, ] <- 0
			}
			ns <- dim(spos)[2]
			if (ns > 0) {
				pos <- rep(0, ns)
				for (i in 1:ns) {
					b <- c(diff(spos[, i]), 1)
					i1 <- is.element(b, 1)
					b[!i1] <- 0
					if (sum(b) == nc) 
					  pos[i] <- spos[1, i]
				}
			}
			else {
				pos <- 0
				pos <- pos[-1]
			}
		}
		pos
	}	

	############################################################################
	# Some definitions and handy variables:
	cmon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
        "Aug", "Sep", "Oct", "Nov", "Dec")
    season <- cbind(c(12, 1, 2), c(3, 4, 5), c(6, 7, 8), c(9, 
        10, 11))
    season.c <- c("", "DJF", "MAM", "JJA", "SON")
    if (!file.exists(filename)) {
        stop(paste("Sorry,", filename, " does not exist!"))
    }
    a <- Sys.info()
    ncid <- open.ncdf(filename)
    nv <- ncid$nvars
    cdfvars <- rep("-", nv)
    for (i in 1:nv) cdfvars[i] <- ncid$var[[i]]$name
    miss <- ncid$var[[1]]$missval
    ipick <- 1
    if (nv > 1) {
        ipick <- grep(v.nam, cdfvars)
        if (length(ipick) == 0) {
            ipick <- as.numeric(readline(paste("Choose variable (1 - ", 
                length(cdfvars), "): ", sep = "")))
        }
    }
    v1 <- cdfvars[ipick]
    nd <- ncid$var[[ipick]]$ndims
    cdfdims <- rep("-", nd)
    for (i in 1:nd) cdfdims[i] <- ncid$var[[ipick]]$dim[[i]]$name
    ilon <- grep(x.nam, tolower(cdfdims))
    ilat <- grep(y.nam, tolower(cdfdims))
    itim <- grep(t.nam, tolower(cdfdims))
    ilev <- grep(z.nam, tolower(cdfdims))
    scal <- NULL
    offs <- NULL
    arv <- att.get.ncdf(ncid, cdfvars[ipick], "scale_factor")
    if (arv$hasatt) 
        scal <- arv$value
    else scal <- 1
    arv <- att.get.ncdf(ncid, cdfvars[ipick], "add_offset")
    if (arv$hasatt) 
        offs <- arv$value
    else offs <- 0
    arv <- att.get.ncdf(ncid, cdfvars[ipick], "units")
    if (arv$hasatt) 
        unit <- arv$value
    else {
        arv <- att.get.ncdf(ncid, cdfvars[ipick], "unit")
        if (arv$hasatt) 
            unit <- arv$value
        else print(paste("Attribute unit not found for", v1))
        unit <- "unknown"
    }
    arv <- att.get.ncdf(ncid, cdfdims[itim], "calendar")
    if (arv$hasatt) 
        calendar <- arv$value
    else calendar <- "ordinary"
    if (!silent) 
        print(calendar)
    if (calendar == "noleap") {
        if (!silent) 
            print("Detected 'noleap' Calendar: set daysayear=365")
        daysayear <- 365
    }
    if ((is.null(torg)) | is.null(t.unit)) {
        if ((tolower(a[1]) == "linux") & (use.cdfcont)) {
            if (!silent) 
                print("Linux & use.cdfcont : call cdfconf()")
            if (is.null(torg)) 
                torg <- cdfcont(filename)$time.origin
            if (is.null(t.unit)) 
                t.unit <- cdfcont(filename)$time.unit
        }
        else {
            arv <- att.get.ncdf(ncid, cdfdims[itim], "time_origin")
            if (arv$hasatt & is.null(torg)) 
                torg <- arv$value
            else {
                if (is.null(torg)) 
                  print("Attribute time_origin not found")
            }
            arv <- att.get.ncdf(ncid, cdfdims[itim], "time_unit")
            if (arv$hasatt & is.null(t.unit)) 
                t.unit <- arv$value
            else {
                arv <- att.get.ncdf(ncid, cdfdims[itim], "unit")
                if (arv$hasatt & is.null(t.unit)) 
                  t.unit <- arv$value
                else {
                  arv <- att.get.ncdf(ncid, cdfdims[itim], "units")
                  if (arv$hasatt & is.null(t.unit)) 
                    t.unit <- arv$value
                }
            }
        }
    }
    lon <- get.var.ncdf(ncid, cdfdims[ilon])
    lat <- get.var.ncdf(ncid, cdfdims[ilat])
    tim <- get.var.ncdf(ncid, cdfdims[itim])
    dt <- tim[2] - tim[1]
    Dt <- tim[length(tim)] - tim[1]
    if (round(length(tim) * dt) != Dt) {
        print(paste("The chronology is not straight forward: dt=", 
            dt, "interval span=", Dt, "data points=", length(tim)))
        tim.srt <- order(tim)
    }
    else tim.srt <- NULL
    attr(lon, "unit") <- eval(parse(text = paste("ncid$dim$'", 
        cdfdims[ilon], "'$units", sep = "")))
    attr(lat, "unit") <- eval(parse(text = paste("ncid$dim$'", 
        cdfdims[ilat], "'$units", sep = "")))
    if (!silent) 
        print(paste("ncid$dim$'", cdfdims[itim], "$units'", sep = ""))
    attr(tim, "unit") <- eval(parse(text = paste("ncid$dim$'", 
        cdfdims[itim], "'$units", sep = "")))
    if (is.null(t.unit)) 
        t.unit <- attr(tim, "unit")
    print(paste("Time, units: ", t.unit))
    if (length(ilev) > 0) {
        if (!is.null(z.rng)) 
            print(paste("Get the levels: ", min(z.rng), max(z.rng)))
        lev <- get.var.ncdf(ncid, cdfdims[ilev])
        attr(lev, "unit") <- eval(parse(text = paste("ncid$dim$", 
            cdfdims[ilev], "$units", sep = "")))
    }
    else {
        lev <- NULL
    }
    arv <- att.get.ncdf(ncid, v1, "long_name")
    if (arv$hasatt) 
        lon.nam <- arv$value
    else lon.nam <- v1
    v.nam <- v1
    start <- rep(1, nd)
    count <- rep(1, nd)
    for (i in 1:nd) count[i] <- eval(parse(text = paste("ncid$dim$'", 
        cdfdims[i], "'$len", sep = "")))
    varsize <- count
    start.test <- start
    count.test = count
    count.test[1:(length(count.test) - 1)] <- 1
    dtim <- diff(tim)
    if (sum(dtim <= 0) > 0) {
        print(paste("Warning! Test of chonological order finds", 
            sum(dtim <= 0), "jump(s)"))
        if (!silent) 
            print(paste("median(dtim)=", median(dtim)))
        nt <- length(tim)
        tim.att <- attributes(tim)
        dtims <- as.numeric(row.names(table(dtim)))
        if (force.chron) {
            if ((length(dtims < 4)) & (is.null(tim.srt))) {
                if (!silent) 
                  print(paste("Force correction: assume tim[1] is correct,", 
                    median(dtim), "is correct time step, and length=", 
                    nt))
                tim <- seq(tim[1], tim[1] + (nt - 1) * dtim[1], 
                  by = median(dtim))
            }
            else {
                dt <- readline("What is the correct time step? (0 leaves tim unchanged)")
                if (dt != 0) 
                  tim <- seq(tim[1], tim[1] + nt - 1, by = dt)
            }
        }
        if (!silent) 
            print(paste("length(tim)=", length(tim), "nt=", nt))
    }
    if ((is.null(torg)) & (regexpr("since", t.unit)[1] > 0)) {
        torg <- substr(t.unit, regexpr("since", t.unit) + 6, 
            nchar(t.unit))
        t.unit <- substr(t.unit, 1, regexpr("since", t.unit) - 
            2)
    }
    if (is.null(torg)) {
        print(paste("Time units:", t.unit, " l=", min(tim[is.finite(tim)]), 
            "-", max(tim[is.finite(tim)])))
        print("Cannot determine the time origin!")
        print("Example format: '15-Dec-1949'")
        print("NCEP reanalysis typically: 01-01-01")
        print("ERA-40 typically: 1900-01-01")
        torg <- readline("I need a time origin: ")
    }
    if (!is.null(torg)) {
        if (!silent) 
            print(paste("torg=", torg))
        yy0 <- datestr2num(torg)[1]
        mm0 <- datestr2num(torg)[2]
        dd0 <- datestr2num(torg)[3]
        if (is.na(dd0)) 
            dd0 <- 1
        if (is.na(mm0)) 
            mm0 <- 15
    }
    else torg <- readline("Give me the time origin (format='15-Dec-1949'):")
    if (!silent) 
        print(paste("Time origin: (year-month-day)", yy0, "-", 
            mm0, "-", dd0))
    if ((yy0[1] == 0) & (forceBC)) {
        if (!silent) 
            print("There is no year zero (Press et al., Numerical recipies)")
        if (!silent) 
            print("'> print(julday(1,1,1)-julday(1,1,-1))' gives 365")
        if (!silent) 
            print("julday wont work unless the time is fixed")
        if (!silent) 
            print("year0 is set to 1, and 365 days is subtracted from tim")
        if (substr(tolower(t.unit), 1, 4) == "hour") {
            tim <- tim - 365 * 24
            t.unit <- "day"
        }
        if (substr(tolower(t.unit), 1, 3) == "day") 
            tim <- tim - 365
        if (substr(tolower(t.unit), 1, 3) == "mon") 
            tim <- tim - 12
        if (substr(tolower(t.unit), 1, 4) == "year") 
            tim <- tim - 1
        yy0 <- 1
    }
    y.test <- data.e <- get.var.ncdf(ncid, v1, start = start.test, 
        count = count.test)
    if ((sum(is.finite(y.test)) > 100) & (daysayear != 360)) {
        ac.gcm <- data.frame(y = y.test, x1 = as.vector(cos(2 * 
            pi * tim/360)), x2 = as.vector(sin(2 * pi * tim/360)))
        ac.real <- data.frame(y = y.test, x1 = as.vector(cos(2 * 
            pi * tim/daysayear)), x2 = as.vector(sin(2 * pi * 
            tim/daysayear)))
        lm.gcm <- lm(y ~ x1 + x2, data = ac.gcm)
        r2.gcm <- summary(lm.gcm)$r.squared
        lm.real <- lm(y ~ x1 + x2, data = ac.real)
        r2.real <- summary(lm.real)$r.squared
        if (force365.25 == -1) {
            if (!silent) 
                print("> > > > FORCING a '360-day' model year! < < < <")
            juldays <- caldat(tim + julday(mm0, dd0, yy0))
            yy <- caldat(juldays)$year
            mm <- caldat(juldays)$month
            dd <- caldat(juldays)$day
            daysayear <- 365.25
            force365.25 <- TRUE
        }
        if (is.finite(r2.gcm) & is.finite(r2.real)) {
            if ((r2.gcm > r2.real) & (length(rownames(table(diff(tim)))) <= 
                2) & ((substr(tolower(t.unit), 1, 3) == "day") | 
                (substr(tolower(t.unit), 1, 4) == "hour")) & 
                !force365.25) {
                print("> > > > Detecting a '360-day' model year! < < < <")
                yy <- yy0 + floor((tim + (mm0 - 1) * 30 + dd0 - 
                  2)/360)
                mm <- mod(mm0 + floor((dd0 + tim - 2)/30) - 1, 
                  12) + 1
                dd <- mod(dd0 + tim - 2, 30) + 1
                daysayear <- 360
            }
        }
    }
    if (!silent) 
        print(paste("Time unit:", tolower(t.unit)))
    if (substr(tolower(t.unit), 1, 3) == "mon") {
        tim <- floor(tim)
        mm <- mod(mm0 + tim - 1, 12) + 1
        yy <- yy0 + floor((tim + mm0 - 1)/12)
        dd <- rep(15, length(tim))
        obj.type <- "monthly.field.object"
    }
    else if (substr(tolower(t.unit), 1, 3) == "yea") {
        tim <- floor(tim)
        mm <- rep(mm0, length(tim))
        yy <- yy0 + tim
        dd <- rep(dd0, length(tim))
        obj.type <- "monthly.field.object"
        t.unit <- "month"
    }
    else if (substr(tolower(t.unit), 1, 3) == "day") {
        if ((yy0 != 0) & (daysayear == 365.25)) 
            mmddyy <- caldat(tim + julday(mm0, dd0, yy0))
        else if (median(diff(tim)) > 29) {
            year <- yy0 + floor(tim/daysayear)
            month <- mm0 + rep(1:12, ceiling(length(year)/12))[1:length(tim)] - 
                1
            day <- rep(15, length(tim))
            mmddyy <- list(day = day, month = month, year = year)
        }
        else stop(paste("There is a problem with the time dimension - I do not know what to do.", 
            "Can be fixed witrh NCO? (http://sf.net/projects/nco)"))
        mm <- mmddyy$month
        yy <- mmddyy$year
        dd <- mmddyy$day
        obj.type <- "daily.field.object"
    }
    else if (substr(tolower(t.unit), 1, 4) == "hour") {
        if (yy0 != 0) 
            mmddyy <- caldat(tim/24 + julday(mm0, dd0, yy0))
        else {
            stop("retrieve.nc: time unit='hour' but no proper initial year provided (year=0 is problematic)")
        }
        mm <- mmddyy$month
        yy <- mmddyy$year
        dd <- mmddyy$day
        tim <- tim/24
        t.unit <- "day"
        obj.type <- "daily.field.object"
    }
    else if (substr(tolower(t.unit), 1, 5) == "minut") {
        if (yy0 != 0) 
            mmddyy <- caldat(tim/(24 * 60) + julday(mm0, dd0, 
                yy0))
        else {
            stop("retrieve.nc: time unit='minute' but no proper initial year provided (year=0 is problematic)")
        }
        mm <- mmddyy$month
        yy <- mmddyy$year
        dd <- mmddyy$day
        tim <- tim/(24 * 60)
        t.unit <- "day"
        obj.type <- "daily.field.object"
    }
    nt <- length(tim)
    if (!is.null(t.rng)) {
        if (!is.character(t.rng)) {
            start[nd] <- t.rng[1]
            if (start[nd] > nt) {
                print("Argument 't.rng':")
                print(t.rng)
                print("Detected an error: start given by 't.rng' exceeds physical record length")
                print("Are you mixing up dates (given as string argument) and index (numerical argument)?")
                stop()
            }
            tim <- tim[t.rng[1]:t.rng[2]]
            attr(tim, "unit") <- eval(parse(text = paste("ncid$dim$", 
                cdfdims[itim], "$units", sep = "")))
            count[nd] <- length(tim)
            yy <- yy[t.rng[1]:t.rng[2]]
            mm <- mm[t.rng[1]:t.rng[2]]
            dd <- dd[t.rng[1]:t.rng[2]]
        }
        else {
            yy.1 <- datestr2num(t.rng[1])[1]
            mm.1 <- datestr2num(t.rng[1])[2]
            dd.1 <- datestr2num(t.rng[1])[3]
            yy.2 <- datestr2num(t.rng[2])[1]
            mm.2 <- datestr2num(t.rng[2])[2]
            dd.2 <- datestr2num(t.rng[2])[3]
            it <- 1:length(tim)
            it1 <- min(it[(yy * 10000 + mm * 100 + dd >= yy.1 * 
                10000 + mm.1 * 100 + dd.1)], na.rm = TRUE)
            it2 <- max(it[(yy * 10000 + mm * 100 + dd <= yy.2 * 
                10000 + mm.2 * 100 + dd.2)], na.rm = TRUE)
            if (!silent) {
                print(c(yy.1, mm.1, dd.1))
                print(c(yy.2, mm.2, dd.2))
                print(c(it1, it2))
            }
            tim <- tim[it1:it2]
            start[nd] <- max(c(1, it1), na.rm = TRUE)
            count[nd] <- length(tim)
            yy <- yy[it1:it2]
            mm <- mm[it1:it2]
            dd <- dd[it1:it2]
        }
    }
    if ((!is.null(z.rng)) & nd == 4) {
        if (lev[1] > lev[2]) 
            start[3] <- min(sum(lev < min(z.rng)), 1)
        else start[3] <- min(sum(lev < min(z.rng)) + 1, 1)
        iz <- (lev >= min(z.rng) & lev <= max(z.rng))
        lev <- lev[iz]
        count[3] <- length(lev)
        nz <- length(lev)
    }
    else if (nd == 4) {
        nz <- sum(is.finite(lev))
        z.rng <- range(lev, na.rm = TRUE)
    }
    else nz <- 1
    if (!silent) 
        print(paste("Latitudes: ", min(lat[is.finite(lat)]), 
            "-", max(lat[is.finite(lat)]), attr(lat, "unit")))
    if (!is.null(y.rng)) {
        if (!silent) 
            print(paste("extract: ", min(y.rng), "-", max(y.rng)))
        iy <- (lat >= min(y.rng) & lat <= max(y.rng))
        start[2] <- min((1:length(lat))[iy])
        count[2] <- sum(iy)
        lat <- lat[iy]
    }
    y.rng <- range(lat)
    if (!silent) 
        print(paste("Longitudes: ", lon[1], "-", lon[length(lon)], 
            attr(lon, "unit")))
    lon.e <- lon
    lon.w <- lon - 360
    if (!is.null(x.rng)) {
        if (!silent) 
            print(paste("extract: ", min(x.rng), "-", max(x.rng)))
        ix.e <- (lon.e >= min((x.rng))) & (lon.e < max((x.rng)))
        ix.w <- (lon.w >= min((x.rng))) & (lon.w <= max((x.rng)))
    }
    else {
        ix.e <- is.finite(lon)
        ix.w <- !is.finite(lon)
    }
    if (!silent) 
        print(paste("Reading", v1))
    if (sum(ix.e) > 1) {
        start.e <- min((1:length(lon))[ix.e])
        stopp.e <- max((1:length(lon))[ix.e])
        start[1] <- start.e
        count[1] <- stopp.e - start.e + 1
        nx.e <- count[1]
        if (!silent) 
            print("read the data from EASTERN hemisphere")
        if (!silent) 
            print(cbind(start, count, varsize))
        data.e <- get.var.ncdf(ncid, v1, start = start, count = count)
        dim(data.e) <- count
        eastern.hemisphere <- TRUE
        LonE <- get.var.ncdf(ncid, cdfdims[ilon], start = start[1], 
            count = count[1])
    }
    else {
        eastern.hemisphere <- FALSE
        nx.e <- 0
    }
    if (sum(ix.w) > 1) {
        start.w <- min((1:length(lon))[ix.w])
        stopp.w <- max((1:length(lon))[ix.w])
        start[1] <- start.w
        count[1] <- stopp.w - start.w + 1
        nx.w <- count[1]
        if (!silent) 
            print("read the data from WESTERN hemisphere")
        if (!silent) 
            print(cbind(start, count, varsize))
        data.w <- get.var.ncdf(ncid, v1, start = start, count = count)
        western.hemisphere <- TRUE
        LonW <- get.var.ncdf(ncid, cdfdims[ilon], start = start[1], 
            count = count[1])
    }
    else {
        western.hemisphere <- FALSE
        nx.w <- 0
        ix.w[] <- FALSE
    }
    Lat <- get.var.ncdf(ncid, cdfdims[ilat], start = start[2], 
        count = count[2])
    if (eastern.hemisphere & western.hemisphere) {
        lon.we <- intersect(lon[ix.w], lon[ix.e])
        if (length(lon.we) > 0) 
            ix.w[is.element(lon, lon.we)] <- FALSE
        lon <- c(LonE, LonW)
    }
    else if (eastern.hemisphere) 
        lon <- LonE
    else if (western.hemisphere) 
        lon <- LonW
    else stop("Error in retrieve.nc - smothing is wrong!")
    x.rng <- range(lon)
    nt <- length(tim)
    ny <- length(lat)
    nx <- length(lon)
    if (nd == 3) {
        dat <- matrix(nrow = nt, ncol = ny * (nx.e + nx.w))
        dim(dat) <- c(nt, ny, nx.e + nx.w)
    }
    else if (nd == 4) {
        dat <- matrix(nrow = nt, ncol = nz * ny * (nx.e + nx.w))
        dim(dat) <- c(nt, nz, ny, nx.e + nx.w)
    }
    if (!silent) {
        print("dim dat:")
        print(dim(dat))
        print(c(nx.e, nx.w, ny, nz, nt))
        print(x.rng)
        if (eastern.hemisphere) 
            print(dim(data.e))
        if (western.hemisphere) 
            print(dim(data.w))
    }
    if (nx.e == 0) 
        eastern.hemisphere <- FALSE
    if (nx.w == 0) 
        western.hemisphere <- FALSE
    if (nd == 3) {
        for (i in 1:nt) {
            if (eastern.hemisphere & western.hemisphere) 
                dat[i, , ] <- t(rbind(matrix(data.e[, , i], nx.e, 
                  ny), matrix(data.w[, , i], nx.w, ny)))
            else if (eastern.hemisphere) 
                dat[i, , ] <- t(matrix(data.e[, , i], nx.e, ny))
            else if (western.hemisphere) 
                dat[i, , ] <- t(matrix(data.w[, , i], nx.w, ny))
        }
    }
    else if (nd == 4) {
        if (!silent) 
            print("4D:")
        dat4 <- dat + NA
        dim(dat4) <- c(nt, nz, ny, nx)
        for (i in 1:nt) {
            if (eastern.hemisphere & western.hemisphere) 
                dat[i, , , ] <- rbind(data.w[, , , i], data[, 
                  , , i])
            else if (eastern.hemisphere) 
                dat[i, , , ] <- data.e[, , , i]
            else if (western.hemisphere) 
                dat[i, , , ] <- data.w[, , , i]
            for (iz in 1:nz) dat4[i, iz, , ] <- t(dat[i, iz, 
                , ])
        }
        dat <- dat4
        rm(dat4)
        gc(reset = TRUE)
        print(dim(dat))
    }
    if (eastern.hemisphere) 
        rm(data.e)
    if (western.hemisphere) 
        rm(data.w)
    gc(reset = TRUE)
    close.ncdf(ncid)
    if (((substr(tolower(t.unit), 1, 4) == "hour") | (substr(tolower(t.unit), 
        1, 3) == "day")) & (max(diff(dd)) == 0)) {
        if (!silent) 
            print("Monthly data, but time unit set to 'hour'/'day'")
        if (!silent) 
            print("Set time unit to month")
        obj.type <- "monthly.field.object"
        dd[] <- 15
        t.unit <- "month"
    }
    if (!is.null((attributes(lat)$unit))) 
        if (attributes(lat)$unit == "degrees_south") 
            lat <- lat * -1
    if (!is.null((attributes(lon)$unit))) 
        if (attributes(lon)$unit == "degrees_west") 
            lon <- lon * -1
    if (!is.null((attributes(lat)$units))) 
        if (attributes(lat)$units == "degrees_south") 
            lat <- lat * -1
    if (!is.null((attributes(lon)$units))) 
        if (attributes(lon)$units == "degrees_west") 
            lon <- lon * -1
    if (greenwich) {
        lon[lon > 180] <- lon[lon > 180] - 360
    }
    print("Sort longs and lats")
    x.srt <- order(lon)
    y.srt <- order(lat)
    lon <- lon[x.srt]
    if (nd == 3) 
        dat <- dat[, , x.srt]
    else {
        dat <- dat[, , , x.srt]
        dim(dat) <- c(length(tim), length(lev), length(lat), 
            length(lon))
    }
    if (lat[length(lat)] < lat[1]) {
        if (nd == 3) 
            dat <- dat[, y.srt, ]
        else {
            dat <- dat[, , y.srt, ]
            dim(dat) <- c(length(tim), length(lev), length(lat), 
                length(lon))
        }
    }
    lat <- lat[y.srt]
    if ((max(mm) > 12) & (max(dd) <= 12)) {
        mm2 <- mm
        mm <- dd
        dd <- mm2
        rm(mm2)
        gc(reset = TRUE)
    }
    if (!silent) 
        print(paste("First & last records:", yy[1], mm[1], dd[1], 
            "&", yy[length(yy)], mm[length(mm)], dd[length(dd)]))
    if (l.scale) {
        if (!silent) 
            print("BEFORE scale adjustment & weeding")
        if (!silent) 
            print(summary(as.vector(dat)))
        if (miss2na) 
            dat[dat == miss] <- NA
        if (sum(is.na(dat)) > 0) 
            print(paste(sum(is.na(dat)), "of", length(dat), " are set to 'NA'; missing value=", 
                miss))
        if (!silent) 
            print("AFTER scale adjustment & weeding")
    }
    if ((l.scale) & !is.null(scal)) {
        if (!silent) 
            print(paste("Scaling: dat <- dat *", scal))
        if (is.finite(scal)) 
            dat <- dat * scal
    }
    if (((l.scale) & !is.null(offs))) {
        if ((offs != 273) & (unit == "deg C")) {
            a <- readline(prompt = "Correct an old bug? (y/n)")
            if (tolower(a) == "y") 
                dat <- dat + offs
        }
        else if (is.finite(offs)) {
            if (!silent) 
                print(paste("Offset: dat <- dat +", offs))
            dat <- dat + offs
        }
    }
    if ((unit == "K") | (unit == "Kelvin") | (unit == "degrees Kelvin") | 
        (unit == "deg K") | (unit == "degK")) {
        dat <- dat - 273
        unit <- "deg C"
    }
    if ((unit == "Pa") | (substr(tolower(unit), 1, 6) == "pascal") | 
        (unit == "N/m^2") | (unit == "N m^{-1}")) {
        dat <- dat/100
        unit <- "hPa"
    }
    if (!silent) 
        print(summary(as.vector(dat)))
    if (!silent) 
        print(paste("dimensions", nt, ny, nx))
    eos <- nchar(v.nam)
    if (instring("-", v.nam) > 0) {
        eos <- instring("-", v.nam) - 1
    }
    else if (instring("_", v.nam) > 0) {
        eos <- instring("_", v.nam) - 1
    }
    v.nam <- substr(v.nam, 1, eos)
    id.x <- matrix(rep(v.nam, ny * nx), ny, nx)
    slash <- instring("/", filename)
    dot <- instring(".", filename)
    id.t <- rep(substr(filename, slash[length(slash)] + 1, dot[length(dot)] - 
        1), nt)
    dat.att <- list(time.unit = t.unit, time.origin = torg, unit = unit, 
        long.name = lon.nam, filename = filename, scale.factor = scal, 
        add.offset = offs, miss = miss, daysayear = daysayear)
    attr(tim, "unit") <- t.unit
    attr(tim, "time_origin") <- torg
    if ((obj.type == "daily.field.object") & (min(diff(tim)) >= 
        28) & (regular) & (sum(is.element(diff(mm), 0)) > 0) & 
        (substr(tolower(t.unit), 1, 3) == "day")) {
        if (!silent) 
            print("Problems detected in date-timeunit Quality Control")
        tim <- 1:length(tim) - 1
        t.unit <- "month"
        attr(tim, "unit") <- "months"
        mm <- mod(mm0 + tim - 1, 12) + 1
        yy <- yy0 + floor((tim + mm0 - 1)/12)
        dd <- rep(15, length(tim))
        obj.type <- "monthly.field.object"
        if (!silent) 
            print("Re-setting the unit to monthly!")
        if (!silent) 
            print(paste("New First & last dates:", yy[1], mm[1], 
                dd[1], "&", yy[length(yy)], mm[length(mm)], dd[length(dd)]))
    }
    retrieve.nc <- list(dat = dat, lon = lon, lat = lat, tim = tim, 
        lev = lev, v.name = v.nam, id.x = id.x, id.t = id.t, 
        yy = yy, mm = mm, dd = dd, n.fld = 1, id.lon = rep(v.nam, 
            nx), id.lat = rep(v.nam, ny), attributes = dat.att, 
        filename = filename)
    class(retrieve.nc) <- c("field", obj.type)
    invisible(retrieve.nc)
}

