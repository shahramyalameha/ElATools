SUBROUTINE startlayout_hmap(namberlay)
   integer                          ::   namberlay

  if (namberlay==1)  THEN

    WRITE (66, "(a)")'    // Heatmap 1 Data'
    WRITE (66, "(a)")'    var heatmapData1 = [{'
  endif 
    
  if (namberlay==2)     THEN
 
    WRITE (66, "(a)")'    // Heatmap 2 Data'
    WRITE (66, "(a)")'    var heatmapData2 = [{'
  endif  
     
  if (namberlay==3)     THEN
 
    WRITE (66, "(a)")'    // Heatmap 3 Data'
    WRITE (66, "(a)")'    var heatmapData3 = [{'
  endif

End SUBROUTINE
!========================
SUBROUTINE startscript_hmap()
    WRITE (66, "(a)")'<script>'
End SUBROUTINE
!========================

SUBROUTINE endscript_hmap()
    WRITE (66, "(a)")'</script>'
    WRITE (66, "(a)")' '
    WRITE (66, "(a)")'</body>'
    WRITE (66, "(a)")'</html>'
End SUBROUTINE
!==============================
SUBROUTINE div_win_hmap(namberlay)
IF (namberlay==1) THEN
    WRITE (66, "(a)")'    <div class="chart-container">'
    WRITE (66, "(a)")'    <div id="heatmap1" class="chart"></div>'
    WRITE (66, "(a)")'    </div>'
ENDIF
IF (namberlay==2) THEN
    WRITE (66, "(a)")'    <div class="chart-container">'
    WRITE (66, "(a)")'    <div id="heatmap1" class="chart"></div>'
    WRITE (66, "(a)")'    <div id="heatmap2" class="chart"></div>'
    WRITE (66, "(a)")'    </div>'
ENDIF
IF (namberlay==3) THEN
    WRITE (66, "(a)")'    <div class="chart-container">'
    WRITE (66, "(a)")'    <div id="heatmap1" class="chart"></div>'
    WRITE (66, "(a)")'    <div id="heatmap2" class="chart"></div>'
    WRITE (66, "(a)")'    <div id="heatmap3" class="chart"></div>'    
    WRITE (66, "(a)")'    </div>'
ENDIF
END SUBROUTINE
!========================
SUBROUTINE endlayoutcolor_hmap(namberlay, palt, namepro, minmaxneg)
   ChARACTER(len=30)                ::   palt, title, mimang
   ChARACTER(len=11)                :: namepro
   integer                          ::   namberlay
   ChARACTER(len=20)                ::   minmaxneg
   if (namepro == "hmpoi") title = "Poisson\'s ratio"
   if (namepro == "hmpugh") title = "Pugh\'s ratio"
   if (namepro == "hmyoung") title = "Young\'s modulus (GPa)"
   if (namepro == "hmbulk") title = "Bulk modulus (GPa)"
   if (namepro == "hmshear") title = "Shear modulus (GPa)"
   if (namepro == "hmcomp") title = "Linear compressibility (1/TPa)"
   if (namepro == "hmhard") title = "Hardness (GPa)"
   if (namepro == "hmpall") title = "Phase velocity"
   if (namepro == "hmgall") title = "Group velocity"
   if (namepro == "hmpfall") title = "Power Flow angle"
   if (namepro == "hmkm") title = "Min. thermal conductivity"



  if (minmaxneg == "max") mimang="Max. Positive:"
  if (minmaxneg == "min") mimang="Min. Positive:"
  if (minmaxneg == "neg") mimang="Negative:"        
  if (minmaxneg == "pm") mimang="P-mode:"
  if (minmaxneg == "sm") mimang="Slow-mode:"
  if (minmaxneg == "fm") mimang="Fast-mode:"
  if (minmaxneg == "non") mimang=" " 
  
     
   if (namberlay==1)  THEN  
    WRITE (66, "(a)")"        type: 'heatmap',"
    WRITE (66, "(3a)")"       colorscale: '",trim(palt),"',"
    WRITE (66, "(a)")"        name: 'Heatmap 1',"
    WRITE (66, "(a)")"  colorbar:{"
    WRITE (66, "(a)")"    len: 1.05,"
    WRITE (66, "(5a)") "        title: '",trim(mimang)," ", trim(title),"',   "
    WRITE (66, "(a)")"    titleside:'right',"
    WRITE (66, "(a)")"    thickness: 20,"
    WRITE (66, "(a)")"    tickfont: {"
    WRITE (66, "(a)")"                family: 'Times New Roman',"
    WRITE (66, "(a)")"                size: 16"
    WRITE (66, "(a)")"              },"
    WRITE (66, "(a)")"    titlefont: {"
    WRITE (66, "(a)")"                 family: 'Times New Roman',"
    WRITE (66, "(a)")"                         size: 16"
    WRITE (66, "(a)")"               }, "                     
    WRITE (66, "(a)")"            },  "  
    WRITE (66, "(a)")"    }];"
   endif
   if (namberlay==2)  THEN  
    WRITE (66, "(a)")"        type: 'heatmap',"
    WRITE (66, "(3a)")"       colorscale: '",trim(palt),"',"
    WRITE (66, "(a)")"        name: 'Heatmap 2',"
    WRITE (66, "(a)")"  colorbar:{"
    WRITE (66, "(a)")"    len: 1.05,"
    WRITE (66, "(5a)") "        title: '",trim(mimang)," ", trim(title),"',   "
    WRITE (66, "(a)")"    titleside:'right',"
    WRITE (66, "(a)")"    thickness: 20,"    
    WRITE (66, "(a)")"    tickfont: {"
    WRITE (66, "(a)")"                family: 'Times New Roman',"
    WRITE (66, "(a)")"                size: 16"
    WRITE (66, "(a)")"              },"
    WRITE (66, "(a)")"    titlefont: {"
    WRITE (66, "(a)")"                 family: 'Times New Roman',"
    WRITE (66, "(a)")"                         size: 16"
    WRITE (66, "(a)")"               }, "                     
    WRITE (66, "(a)")"            },   "  
    WRITE (66, "(a)")"    }];"
   endif   
   if (namberlay==3)  THEN  
    WRITE (66, "(a)")"        type: 'heatmap',"
    WRITE (66, "(3a)")"       colorscale: '",trim(palt),"',"
    WRITE (66, "(a)")"        name: 'Heatmap 3',"
    WRITE (66, "(a)")"  colorbar:{"
    WRITE (66, "(a)")"    len: 1.05,"
   WRITE (66, "(5a)") "        title: '",trim(mimang)," ", trim(title),"',   "
    WRITE (66, "(a)")"    titleside:'right',"
    WRITE (66, "(a)")"    thickness: 20,"    
    WRITE (66, "(a)")"    tickfont: {"
    WRITE (66, "(a)")"                family: 'Times New Roman',"
    WRITE (66, "(a)")"                size: 16"
    WRITE (66, "(a)")"              },"
    WRITE (66, "(a)")"    titlefont: {"
    WRITE (66, "(a)")"                 family: 'Times New Roman',"
    WRITE (66, "(a)")"                         size: 16"
    WRITE (66, "(a)")"               }, "                     
    WRITE (66, "(a)")"            },   "  
    WRITE (66, "(a)")"    }];"
   endif  
end SUBROUTINE    
