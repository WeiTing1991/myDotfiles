Vim�UnDo� bλ���͔��Lm��q1���R�K��'h��      }                             e�.    _�                             ����                                                                                                                                                                                                                                                                                                                                                             e�~�     �               {5��                          
                      �                       
                  
       �       	                                        �                                             �       	                                        �                                               �                                               �                                               �                                               �                                               �                                               �                                               �                                               �                                                �                          
                      5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             e�     �                 }5��                         �                     �                        �                    �                         �                     5�_�                             ����                                                                                                                                                                                                                                                                                                                                                             e�-    �                    return {�      
         {   		"L3MON4D3/LuaSnip",   		event = "InsertEnter",   		-- follow latest release.   k		version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)   "		-- install jsregexp (optional!).   "		build = "make install_jsregexp",�   
            		dependencies = {   4			"saadparwaiz1/cmp_luasnip", -- for autocompletion   5			"rafamadriz/friendly-snippets", -- useful snippets   		},�            	   	{   		"hrsh7th/nvim-cmp",   		event = "InsertEnter",   		dependencies = {   5			"hrsh7th/cmp-buffer", -- source for text in buffer   6			"hrsh7th/cmp-path", -- source for file system paths   			"hrsh7th/cmp-nvim-lsp",   5			"onsails/lspkind.nvim", -- vs-code like pictograms   		},�                 }5��                          �                     �                    8   �                  �    
                8   
      �       �       �                       
       �       �       �                                                  5��